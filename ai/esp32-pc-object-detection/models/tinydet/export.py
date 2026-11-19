"""INT8 export, TFLite evaluation and firmware metadata.

Ported from the working notebook cells (Conv+BN fusion -> ONNX opset 13 -> onnx2tf
SavedModel -> TFLiteConverter full INT8), with three changes:
- fusion walks every Sequential and timm block, so all head types and the context
  branch fuse;
- the calibration set is shuffled and positive-heavy (``data.calibration_items``);
- ``evaluate_tflite`` scores the INT8 model on the same splits and metrics as training,
  quantising probabilities to /255 like the firmware so plateau ties behave the same.

TensorFlow and onnx2tf are imported only inside these functions.
"""

from __future__ import annotations

import copy
import json
import shutil
from datetime import datetime, timezone
from pathlib import Path

import numpy as np
import torch
import torch.nn as nn
from torch.nn.utils.fusion import fuse_conv_bn_eval

from . import data as D
from . import evaluate as E
from . import models as M
from . import targets as T
from .config import Config


def load_trial(trial_dir: str | Path, rung: int | None = None):
    """(model, cfg, checkpoint) for a trial's latest (or given) rung weights."""
    trial_dir = Path(trial_dir)
    if rung is None:
        rungs = sorted(int(p.stem[4:]) for p in trial_dir.glob("rung*.pt"))
        if not rungs:
            raise FileNotFoundError(f"no rung*.pt in {trial_dir}")
        rung = rungs[-1]
    ck = torch.load(trial_dir / f"rung{rung}.pt", map_location="cpu", weights_only=False)
    cfg = Config(ck["config"], strict=False)
    model = M.build_model(cfg, pretrained=False)
    model.load_state_dict(ck["model"])
    return model.eval(), cfg, ck


# ── Fusion ──────────────────────────────────────────────────────────────────

def _act_of(bn):
    return getattr(bn, "act", None) or nn.Identity()


def _fuse_seq(seq: nn.Sequential) -> nn.Sequential:
    out, i, mods = [], 0, list(seq)
    while i < len(mods):
        a = mods[i]
        if i + 1 < len(mods) and isinstance(a, nn.Conv2d) and isinstance(mods[i + 1], nn.BatchNorm2d):
            bn = mods[i + 1]
            out.append(fuse_conv_bn_eval(a, bn))
            act = _act_of(bn)
            if not isinstance(act, nn.Identity):
                out.append(act)
            i += 2
        else:
            out.append(_fuse_any(a))
            i += 1
    return nn.Sequential(*out)


def _fuse_any(m: nn.Module) -> nn.Module:
    if isinstance(m, nn.Sequential):
        return _fuse_seq(m)
    # timm InvertedResidual: conv_pw -> bn1, conv_dw -> bn2, conv_pwl -> bn3
    if hasattr(m, "conv_pwl"):
        for conv, bn in (("conv_pw", "bn1"), ("conv_dw", "bn2"), ("conv_pwl", "bn3")):
            if isinstance(getattr(m, bn), nn.BatchNorm2d):
                setattr(m, conv, fuse_conv_bn_eval(getattr(m, conv), getattr(m, bn)))
                setattr(m, bn, _act_of(getattr(m, bn)))
        return m
    # timm DepthwiseSeparableConv: conv_dw -> bn1, conv_pw -> bn2
    if hasattr(m, "conv_dw") and hasattr(m, "conv_pw"):
        for conv, bn in (("conv_dw", "bn1"), ("conv_pw", "bn2")):
            if isinstance(getattr(m, bn), nn.BatchNorm2d):
                setattr(m, conv, fuse_conv_bn_eval(getattr(m, conv), getattr(m, bn)))
                setattr(m, bn, _act_of(getattr(m, bn)))
        return m
    for name, child in list(m.named_children()):
        setattr(m, name, _fuse_any(child))
    return m


def fuse_for_export(model: nn.Module, check_input_size: int | None = None, in_ch: int = 3) -> nn.Module:
    m = copy.deepcopy(model).cpu().eval()
    for p in m.parameters():
        p.requires_grad_(False)
    m = _fuse_any(m)
    remaining = [n for n, x in m.named_modules() if isinstance(x, nn.BatchNorm2d)]
    if remaining:
        raise RuntimeError(f"unfused BatchNorm layers: {remaining[:5]}")
    if check_input_size:
        x = torch.randn(1, in_ch, check_input_size, check_input_size)
        with torch.no_grad():
            diff = (model.cpu().eval()(x) - m(x)).abs().max().item()
        if diff > 1e-4:
            raise RuntimeError(f"fusion changed the output by {diff:.2e}")
    return m


def apply_platt(model: nn.Module, a: float, b: float) -> nn.Module:
    """Fold Platt scaling into the last conv: logits' = a * logits + b. Exact, zero cost.

    Peaks are unchanged (monotone), but the firmware's sigmoid then reports calibrated
    probabilities, so a threshold of 0.6 means "about 60% of detections here are real".
    """
    m = copy.deepcopy(model)
    conv = m.head[-1]
    with torch.no_grad():
        conv.weight.mul_(a)
        conv.bias.mul_(a).add_(b)
    return m


class Clamped(nn.Module):
    """Optional output clamp, to give INT8 more steps near the threshold."""

    def __init__(self, model, lo: float, hi: float):
        super().__init__()
        self.model, self.lo, self.hi = model, lo, hi

    def forward(self, x):
        return torch.clamp(self.model(x), self.lo, self.hi)


def export_onnx(model: nn.Module, cfg: Config, path: str | Path, clamp: tuple | None = None) -> Path:
    s = cfg["data.input_size"]
    fused = fuse_for_export(model, check_input_size=s, in_ch=cfg.in_channels)
    if clamp:
        fused = Clamped(fused, *clamp)
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    kw = dict(opset_version=13, do_constant_folding=True, input_names=["input"],
              output_names=["output"], dynamic_axes=None)
    x = torch.randn(1, cfg.in_channels, s, s)
    try:
        torch.onnx.export(fused, x, str(path), dynamo=False, **kw)
    except TypeError:
        torch.onnx.export(fused, x, str(path), **kw)
    print(f"ONNX: {path} ({path.stat().st_size / 1024:.1f} KB)")
    return path


# ── Calibration and conversion ──────────────────────────────────────────────

def calibration_array(cfg: Config, index: dict, splits: dict, n: int = 400, seed: int = 0) -> np.ndarray:
    """Normalised NHWC float32 calibration images (positive-heavy, shuffled)."""
    items = D.calibration_items(splits, n=n, seed=seed)
    ds = D.DetectionDataset(index, items, cfg["data.coco_root"], cfg.class_ids, cfg["data.input_size"], train=False)
    prep = M.InputPrep(cfg, torch.device("cpu"))
    out = []
    for i in range(len(ds)):
        img, _ = ds[i]
        out.append(prep(img[None]).permute(0, 2, 3, 1).numpy())
    return np.concatenate(out).astype(np.float32)


def convert_int8(onnx_path: str | Path, out_dir: str | Path, calib: np.ndarray) -> Path:
    """ONNX -> SavedModel (onnx2tf) -> full-integer INT8 .tflite (TFLiteConverter)."""
    import onnx2tf
    import tensorflow as tf
    out_dir = Path(out_dir)
    if out_dir.exists():
        shutil.rmtree(out_dir)
    onnx2tf.convert(input_onnx_file_path=str(onnx_path), output_folder_path=str(out_dir),
                    tflite_backend="tf_converter", output_signaturedefs=True,
                    copy_onnx_input_output_names_to_tflite=True, non_verbose=True)
    conv = tf.lite.TFLiteConverter.from_saved_model(str(out_dir))
    conv.optimizations = [tf.lite.Optimize.DEFAULT]
    conv.representative_dataset = lambda: ([calib[i:i + 1]] for i in range(len(calib)))
    conv.target_spec.supported_ops = [tf.lite.OpsSet.TFLITE_BUILTINS_INT8]
    conv.inference_input_type = tf.int8
    conv.inference_output_type = tf.int8
    path = out_dir / "model_int8.tflite"
    path.write_bytes(conv.convert())
    print(f"INT8 TFLite: {path} ({path.stat().st_size / 1024:.1f} KB)")
    return path


class TFLiteModel(nn.Module):
    """Wraps an INT8 .tflite so ``evaluate.evaluate`` can call it like a torch model.

    Input: normalised NCHW float batch. Output: dequantised NCHW logits.
    """

    def __init__(self, path: str | Path, firmware_scores: bool = True):
        super().__init__()
        import tensorflow as tf
        self.interp = tf.lite.Interpreter(model_path=str(path))
        self.interp.allocate_tensors()
        self.inp = self.interp.get_input_details()[0]
        self.out = self.interp.get_output_details()[0]
        self.firmware_scores = firmware_scores

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        xs = x.float().permute(0, 2, 3, 1).cpu().numpy()
        si, zi = self.inp["quantization"]
        so, zo = self.out["quantization"]
        outs = []
        for i in range(xs.shape[0]):
            q = np.clip(np.round(xs[i:i + 1] / si + zi), -128, 127).astype(np.int8)
            self.interp.set_tensor(self.inp["index"], q)
            self.interp.invoke()
            y = (self.interp.get_tensor(self.out["index"]).astype(np.float32) - zo) * so
            outs.append(y)
        logits = torch.from_numpy(np.concatenate(outs)).permute(0, 3, 1, 2)
        if self.firmware_scores:  # firmware compares round(sigmoid * 255) values
            p = torch.round(torch.sigmoid(logits) * 255) / 255
            logits = torch.logit(p.clamp(0.5 / 255, 1 - 0.5 / 255))
        return logits


def evaluate_tflite(path: str | Path, cfg: Config, ctx, splits_to_run=D.EVAL_SPLITS,
                    report_thr: float = 0.3) -> dict:
    tfl = TFLiteModel(path)
    prep = M.InputPrep(cfg, torch.device("cpu"))
    metrics = {}
    for name in splits_to_run:
        m = E.evaluate({"m": (tfl, prep)}, ctx.eval_loader(name, cfg), cfg, torch.device("cpu"), report_thr)["m"]
        metrics.update({f"{name}/{k}": v for k, v in m.items()})
    return metrics


# ── Firmware ────────────────────────────────────────────────────────────────

def write_firmware(tflite_path: str | Path, cfg: Config, firmware_model_dir: str | Path,
                   training: dict | None = None, conf_threshold: float = 0.3) -> Path:
    """Copy the model next to a format-v2 model_metadata.json that firmware/build.rs checks."""
    import tensorflow as tf
    d = Path(firmware_model_dir)
    d.mkdir(parents=True, exist_ok=True)
    dest = d / "fomo_model_int8.tflite"
    shutil.copy2(tflite_path, dest)
    it = tf.lite.Interpreter(model_path=str(dest))

    def meta(t):
        n, h, w, c = (int(v) for v in t["shape"])
        q = t["quantization_parameters"]
        assert n == 1 and t["dtype"] == np.int8 and len(q["scales"]) == 1, t
        return {"width": w, "height": h, "channels": c, "dtype": "int8", "layout": "NHWC",
                "scale": float(q["scales"][0]), "zero_point": int(q["zero_points"][0])}

    inp, out = meta(it.get_input_details()[0]), meta(it.get_output_details()[0])
    assert (inp["width"], inp["channels"]) == (cfg["data.input_size"], cfg.in_channels)
    assert out["channels"] == cfg.num_classes
    mean, std = cfg.norm_stats()
    md = {
        "format_version": 2, "model_file": dest.name, "model_size_bytes": dest.stat().st_size,
        "exported_at": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "input": inp, "output": out, "grid_stride": cfg["data.cell_size"],
        "class_names": ["background"] + cfg.class_names, "output_activation": "sigmoid",
        "normalization": {"mean": mean, "std": std}, "conf_threshold": conf_threshold,
        "training": {**cfg.to_dict(), **(training or {})},
    }
    (d / "model_metadata.json").write_text(json.dumps(md, indent=2, default=str) + "\n")
    print(f"firmware model: {dest} ({md['model_size_bytes'] / 1024:.1f} KB) + model_metadata.json")
    return dest
