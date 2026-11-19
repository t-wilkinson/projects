"""End-to-end self-check for tinydet, no COCO needed.

    cd models
    python tests/smoke_test.py            # unit checks + a tiny search on synthetic data (~2-5 min)
    python tests/smoke_test.py --export   # also ONNX -> INT8 TFLite -> firmware metadata (needs TensorFlow)

Uses the GPU when available. Everything is written under tests/_smoke/ (safe to delete).
Exits non-zero on the first failure.
"""

from __future__ import annotations

import argparse
import copy
import json
import shutil
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))

import torch  # noqa: E402

import tinydet as td  # noqa: E402
from tinydet.config import Config  # noqa: E402

import synthetic_coco  # noqa: E402

OK = []


def check(name, cond, detail=""):
    if not cond:
        print(f"FAIL  {name} {detail}")
        sys.exit(1)
    OK.append(name)
    print(f"ok    {name} {detail}")


def unit_checks():
    import numpy as np
    from tinydet import evaluate as E, models as M, targets as T

    boxes = torch.tensor([[[0, 44.2, 60.0, 6.0, 6.0, 1]]])
    h = T.render(boxes, 1, 120, 8, T.TargetSpec(form="gauss_cell", sigma=0.7))["heatmap"][0, 0]
    check("targets: cell-centred Gaussian is symmetric", abs(float(h[7, 4] - h[7, 6])) < 1e-6 and float(h[7, 5]) == 1.0)
    b = torch.tensor([[[0, 50, 50, 3, 3, 1], [0, 10, 10, 20, 20, 2], [0, 0, 40, 5, 14, 1], [0, 80, 80, 10, 10, 1]]],
                     dtype=torch.float32)
    pos, ign = T.box_roles(b, 120, T.TargetSpec())
    check("targets: tiny / crowd / clipped boxes ignored", pos.tolist() == [[False, False, False, True]]
          and ign.tolist() == [[True, True, True, False]])

    s = torch.zeros(1, 1, 5, 5)
    s[0, 0, 2, 1] = s[0, 0, 2, 2] = 0.6
    check("eval: firmware tie rule gives one peak per plateau", len(E.decode_peaks(s, 0.3)[0]) == 1)

    rng = np.random.default_rng(0)
    p = rng.uniform(0.05, 1, 20000)
    y = rng.random(20000) < p
    z = np.log(p / (1 - p))
    c = E.calibration(1 / (1 + np.exp(-3 * z)), y, np.arange(20000) % 500)
    check("eval: Platt scaling fixes overconfidence", c["ece_cal"] < 0.03 and abs(c["platt_a"] - 1 / 3) < 0.05,
          f"(ece {c['ece']:.3f} -> {c['ece_cal']:.3f})")

    import timm
    full = timm.create_model("mobilenetv2_050", pretrained=False).eval()
    full.bn1.running_mean.normal_()
    x01 = torch.rand(1, 3, 32, 32)
    m = torch.tensor(M.IMAGENET_MEAN).view(1, 3, 1, 1)
    sd = torch.tensor(M.IMAGENET_STD).view(1, 3, 1, 1)
    with torch.no_grad():
        ref = full.bn1(full.conv_stem((x01 - m) / sd))
        conv, bn = copy.deepcopy(full.conv_stem), copy.deepcopy(full.bn1)
        out = bn(M._reparam_stem(conv, bn, [0.5] * 3, [0.5] * 3, gray=False)((x01 - 0.5) / 0.5))
    check("models: stem re-parameterisation is exact", float((ref - out)[..., 1:-1, 1:-1].abs().max()) < 1e-4)

    for head in ("basic", "dw1", "dwsep", "bottleneck", "dilated"):
        td.export.fuse_for_export(M.build_model(Config({"model.head": head, "model.pretrained": False}), pretrained=False),
                                  check_input_size=120)
    td.export.fuse_for_export(M.build_model(Config({"model.context": True, "model.pretrained": False}), pretrained=False),
                              check_input_size=120)
    check("export: Conv+BN fusion exact for every head and the context branch", True)
    pre = td.models.has_pretrained(0.5)
    check("models: timm knows ImageNet weights for width 0.5", pre)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--export", action="store_true", help="also test ONNX/TFLite export (needs TensorFlow)")
    ap.add_argument("--keep", action="store_true", help="keep tests/_smoke from a previous run")
    args = ap.parse_args()
    work = HERE / "_smoke"
    if work.exists() and not args.keep:
        shutil.rmtree(work)
    work.mkdir(exist_ok=True)
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    print(f"device: {device} {torch.cuda.get_device_name(0) if device.type == 'cuda' else ''}")
    t0 = time.time()

    unit_checks()

    coco = synthetic_coco.make(work / "coco")
    index = td.data.load_index(coco, [37], cache_dir=work / "cache")
    splits = td.data.build_splits(index, cache_dir=work / "cache", val_neg_n=40)
    check("data: index + fixed splits", len(splits["train_pos"]) > 50 and len(splits["val_zoom"]) > 50)
    ctx = td.train.DataContext(index, splits, str(coco), workers=2, eval_batch=64, cache_dir=work / "cache")

    cfg = Config({"data.coco_root": str(coco), "data.device_strength": 0.9, "data.device_motion": 0.6,
                  "data.device_lens": "wide", "data.paste_prob": 1.0, "data.paste_max": 3,
                  "data.paste_bank_min_px": 12, "data.aug": "strong", "data.zoom_prob": 0.5})
    ds = td.data.train_dataset(cfg, index, splits, bank=ctx.cutout_bank(12))
    n_boxes = 0
    for i in range(len(ds)):
        im, bx = ds[i]
        assert im.shape == (3, 120, 120) and im.dtype == torch.uint8
        if bx.shape[0]:
            assert (bx[:, 3] > 0).all() and (bx[:, 1] + bx[:, 3] <= 120.01).all()
        n_boxes += bx.shape[0]
    check("data: full augmentation (camera sim + copy-paste + zoom) on every image", n_boxes > len(ds),
          f"({len(ds)} images, {n_boxes} boxes)")
    dev = td.data.eval_dataset("val_dev", cfg, index, splits)
    check("data: val_dev is deterministic", bool((dev[3][0] == dev[3][0]).all()))

    # tiny search derived from the real space.json: same params/objective, tiny budgets
    space = json.loads((HERE.parent / "search" / "space.json").read_text())
    space["name"] = "smoke"
    space["schedule"].update({"rungs": [2, 4], "eta": 2, "group_size": 2, "eval_every": 1})
    space["base"] = {"data.coco_root": str(coco), "model.pretrained": False, "optim.batch_size": 16,
                     "optim.warmup_epochs": 1, "data.paste_bank_min_px": 12}
    space["params"]["model.pretrained"]["stage"] = "off"
    for st in space["stages"]:
        st["trials"] = {"baseline": 2, "structure": 3, "domain": 3}.get(st["name"], 0)
        st["startup"] = 2
        if st["name"] == "baseline":
            st["seeds"] = [{}, {"target.form": "gauss_peak", "target.legacy_offset": True}]
    (work / "space.json").write_text(json.dumps(space, indent=2))
    search = td.search.Search(work / "exp", work / "space.json", ctx, device=device, seed=0)
    search.run()
    st = search.state()
    closed = [s["name"] for s in space["stages"] if s["name"] in st.closed]
    scored = [t for t in st.trials.values() if td.search.score(t) is not None]
    check("search: every stage closed", len(closed) == len(space["stages"]), f"({len(st.trials)} trials)")
    check("search: trials scored with the current objective", len(scored) >= 6)
    best = search.leaderboard(1).iloc[0]
    m = st.trials[best["trial"]]["rungs"][td.search.top_rung(st.trials[best["trial"]])]["metrics"]
    check("eval: calibration metrics present", "val_zoom/ece_cal" in m and "val_dev/ap" in m)
    dash = td.dashboard.build(work / "exp", work / "space.json")
    check("dashboard: built", dash.exists() and dash.stat().st_size > 10_000, f"({dash})")

    res = search.manual({"model.head": "bottleneck"}, epochs=2)
    check("search: manual run", res.get("objective") is not None)

    if args.export:
        model, bcfg, ck = td.export.load_trial(work / "exp" / "trials" / best["trial"])
        bcfg = bcfg.override({"data.coco_root": str(coco)})
        model = td.export.apply_platt(model, ck["metrics"]["val_zoom/platt_a"], ck["metrics"]["val_zoom/platt_b"])
        onnx = td.export.export_onnx(model, bcfg, work / "export" / "model.onnx")
        tfl = td.export.convert_int8(onnx, work / "export" / "tf", td.export.calibration_array(bcfg, index, splits, n=48))
        q = td.export.evaluate_tflite(tfl, bcfg, ctx, splits_to_run=("val_zoom",))
        check("export: INT8 TFLite evaluates", q.get("val_zoom/ap") is not None,
              f"(float AP {ck['metrics']['val_zoom/ap']:.3f}, INT8 AP {q['val_zoom/ap']:.3f}, INT8 ECE {q.get('val_zoom/ece', float('nan')):.3f})")
        td.export.write_firmware(tfl, bcfg, work / "export" / "firmware_model")
        check("export: firmware metadata written", (work / "export" / "firmware_model" / "model_metadata.json").exists())

    print(f"\nALL {len(OK)} CHECKS PASSED in {time.time() - t0:.0f} s")


if __name__ == "__main__":
    main()
