"""Flat dotted-key configuration.

Every tunable setting has one key such as ``model.width`` or ``target.sigma``. The
same keys are used by ``search/space.json``, the run ledger and the dashboard, so a
trial's parameters can be read, diffed and re-run without translation.

Keys under ``data.*`` plus ``optim.batch_size`` form the *data signature*: trials with
the same signature can be trained in lockstep on the same batches (see train.py).
Everything else (model, targets, loss, optimiser, input normalisation, grayscale) is
applied per model on the GPU, so it does not split groups.
"""

from __future__ import annotations

import copy
import hashlib
import json
from pathlib import Path
from typing import Any, Iterable

DEFAULTS: dict[str, Any] = {
    # ── Data pipeline (shared by a lockstep group) ────────────────────────────
    "data.coco_root": "~/datasets/coco",       # full-resolution COCO (keeps aspect ratio)
    "data.classes": {"37": "sports_ball"},     # COCO category id -> name, background implicit
    "data.input_size": 120,                    # square model input, pixels
    "data.cell_size": 8,                       # backbone stride = grid cell size
    "data.bg_ratio": 0.4,                      # fraction of training images without targets
    "data.bg_source": "random",                # random | confusers | indoor | mixed (50/50 random/confusers)
    "data.zoom_prob": 0.5,                     # chance a positive image is cropped around one target
    "data.zoom_px": [6, 32],                   # target diameter after a zoom crop, input pixels
    "data.aug": "light",                       # light | strong: generic photometric augmentation
    # Camera simulation (camera.py): the ESP32-S3 / OV2640 pipeline. 0 / "none" = off.
    "data.device_strength": 0.0,               # sensor noise, exposure/AGC, white balance, ISP sharpening, vignetting, RGB565
    "data.device_motion": 0.0,                 # car motion: blur, rolling-shutter shear and wobble
    "data.device_lens": "none",                # none | mild (stock ~66 deg) | wide (120-160 deg fisheye)
    # Copy-paste (paste.py): crisp COCO ball cutouts pasted at the car's working sizes
    "data.paste_prob": 0.0,                    # chance a training image gets pasted balls
    "data.paste_max": 2,                       # up to this many balls per image
    "data.paste_px": [6, 36],                  # pasted ball diameter, input pixels (log-uniform)
    "data.paste_floor_bias": 0.5,              # 0 = anywhere, 1 = strongly toward the bottom (floor) + contact shadows
    "data.paste_bank_min_px": 20,              # only balls at least this big in their source image become cutouts
    # Your own ball-free frames (e.g. from the car) added as backgrounds
    "data.extra_bg_dir": None,
    "data.extra_bg_repeat": 1,
    "data.workers": 8,
    # ── Per-model input handling (done on GPU) ────────────────────────────────
    "model.grayscale": False,                  # luma from RGB like the firmware (77R+150G+29B)>>8
    "model.norm": "imagenet",                  # imagenet | symmetric (x/127.5-1)
    # ── Model ─────────────────────────────────────────────────────────────────
    "model.width": 0.5,                        # MobileNetV2 width multiplier
    "model.pretrained": True,                  # ImageNet weights (only where they exist)
    "model.tap": True,                         # take the next block's 1x1 expansion at stride 8
    "model.context": False,                    # add upsampled stride-16 features at stride 8
    "model.head": "dwsep",                     # basic | dw1 | dwsep | bottleneck | dilated
    "model.head_ch": None,                     # head width; None = the head's default
    "model.dropout": 0.1,
    "model.freeze": "early",                   # early (stem + first two blocks) | none
    "model.unfreeze_epoch": -1,                # epoch to unfreeze frozen layers, -1 = never
    "model.backbone_lr_mult": 1.0,             # LR multiplier for backbone parameters
    # ── Targets ───────────────────────────────────────────────────────────────
    "target.form": "gauss_cell",               # gauss_peak | gauss_cell | onehot | onehot_nbr_ignore
    "target.sigma": 0.7,                       # Gaussian sigma in cells (floor when sigma_scale > 0)
    "target.sigma_scale": 0.0,                 # sigma = max(sigma, sigma_scale * diameter_in_cells)
    "target.ignore_px": 3.5,                   # boxes with w*h <= ignore_px**2 are ignored, not negatives
    "target.ignore_footprint": 3,              # ignored square around a tiny/clipped box centre (cells)
    "target.clipped_aspect": 0.6,              # border-touching boxes with min/max side below this are ignored
    "target.legacy_offset": False,             # True reproduces the old half-cell Gaussian offset bug
    # ── Loss ──────────────────────────────────────────────────────────────────
    "loss.name": "centernet",                  # centernet | bce | sigmoid_focal
    "loss.alpha": 2.0,                         # centernet: focusing power; sigmoid_focal: gamma
    "loss.beta": 4.0,                          # centernet: penalty reduction near centres
    "loss.pos_weight": 50.0,                   # bce: weight on positive cells
    # ── Optimisation ──────────────────────────────────────────────────────────
    "optim.batch_size": 64,
    "optim.lr": 2e-3,
    "optim.wd": 1e-4,
    "optim.warmup_epochs": 3,
    "optim.schedule": "wsd",                   # wsd (warmup-stable-decay) | cosine
    "optim.decay_frac": 0.2,                   # wsd: decay branch length as a fraction of the rung
    "optim.min_lr_frac": 0.01,
    "optim.ema": 0.998,                        # EMA decay, 0 disables
    "optim.grad_clip": 5.0,
    "optim.amp": True,                         # bf16 autocast on CUDA
    "seed": 0,                                 # model init seed (data order comes from the group)
}

DATA_SIGNATURE_PREFIXES = ("data.",)
DATA_SIGNATURE_EXTRA = ("optim.batch_size",)
# Keys that never change results (paths, worker count) are excluded from hashes.
NON_RESULT_KEYS = ("data.coco_root", "data.workers")


class Config:
    """A flat mapping of dotted keys to JSON-serialisable values.

    >>> cfg = Config({"model.width": 0.75})
    >>> cfg["model.width"], cfg["model.head"]
    (0.75, 'dwsep')
    >>> cfg2 = cfg.override({"target.sigma": 0.5})
    """

    def __init__(self, values: dict[str, Any] | None = None, strict: bool = True):
        self._v = copy.deepcopy(DEFAULTS)
        if values:
            for k, v in values.items():
                if strict and k not in DEFAULTS:
                    raise KeyError(f"unknown config key {k!r}; known keys: {sorted(DEFAULTS)}")
                self._v[k] = copy.deepcopy(v)

    # mapping protocol
    def __getitem__(self, key: str) -> Any:
        return self._v[key]

    def __contains__(self, key: str) -> bool:
        return key in self._v

    def get(self, key: str, default: Any = None) -> Any:
        return self._v.get(key, default)

    def keys(self) -> Iterable[str]:
        return self._v.keys()

    def items(self):
        return self._v.items()

    def to_dict(self) -> dict[str, Any]:
        return copy.deepcopy(self._v)

    def override(self, values: dict[str, Any] | None = None, **kw) -> "Config":
        merged = self.to_dict()
        merged.update(values or {})
        merged.update({k.replace("__", "."): v for k, v in kw.items()})
        return Config(merged)

    def diff(self, base: "Config | None" = None) -> dict[str, Any]:
        """Keys whose value differs from ``base`` (defaults when None)."""
        ref = base.to_dict() if base else DEFAULTS
        return {k: v for k, v in self._v.items() if ref.get(k) != v}

    def section(self, prefix: str) -> dict[str, Any]:
        """``cfg.section('model')`` -> {'width': 0.5, ...}"""
        p = prefix.rstrip(".") + "."
        return {k[len(p):]: v for k, v in self._v.items() if k.startswith(p)}

    # identity
    def hash(self, keys: Iterable[str] | None = None, n: int = 10) -> str:
        keys = sorted(keys if keys is not None else self._v.keys())
        payload = {k: self._v[k] for k in keys if k not in NON_RESULT_KEYS}
        blob = json.dumps(payload, sort_keys=True, default=str).encode()
        return hashlib.sha1(blob).hexdigest()[:n]

    def data_signature(self) -> str:
        keys = [k for k in self._v if k.startswith(DATA_SIGNATURE_PREFIXES) or k in DATA_SIGNATURE_EXTRA]
        return self.hash(keys, n=8)

    # convenience
    @property
    def grid_size(self) -> int:
        return self._v["data.input_size"] // self._v["data.cell_size"]

    @property
    def class_names(self) -> list[str]:
        return list(self._v["data.classes"].values())

    @property
    def class_ids(self) -> list[int]:
        return [int(k) for k in self._v["data.classes"].keys()]

    @property
    def num_classes(self) -> int:
        return len(self._v["data.classes"])

    @property
    def in_channels(self) -> int:
        return 1 if self._v["model.grayscale"] else 3

    def norm_stats(self) -> tuple[list[float], list[float]]:
        """Mean/std applied to [0, 1] pixels, one entry per input channel."""
        gray = self._v["model.grayscale"]
        if self._v["model.norm"] == "symmetric":
            return ([0.5], [0.5]) if gray else ([0.5] * 3, [0.5] * 3)
        if self._v["model.norm"] == "imagenet":
            return ([0.449], [0.226]) if gray else ([0.485, 0.456, 0.406], [0.229, 0.224, 0.225])
        raise ValueError(f"unknown model.norm {self._v['model.norm']!r}")

    def save(self, path: str | Path) -> None:
        Path(path).write_text(json.dumps(self._v, indent=2, default=str) + "\n")

    @classmethod
    def load(cls, path: str | Path) -> "Config":
        return cls(json.loads(Path(path).read_text()), strict=False)

    def __repr__(self) -> str:
        d = self.diff()
        return f"Config({d})" if d else "Config(defaults)"
