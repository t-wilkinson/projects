"""COCO index cache, fixed evaluation splits, datasets and augmentation pipelines.

Design notes
- Annotations are read once with plain ``json`` and cached as a small index file, so a
  notebook restart costs ~1 s instead of ~20 s of ``COCO(...)`` loading.
- Splits are built once from a seed and saved; they never depend on a trial's settings.
  Background pools are pre-shuffled, so ``data.bg_ratio`` just takes a longer prefix.
- Datasets return **uint8** CHW images and raw boxes. Normalisation, grayscale and
  target rendering happen per model on the GPU (``targets.py`` / ``train.py``), which
  lets several trials with different target or model settings share one batch stream.
- Every box is kept (no ``min_visibility`` filtering). Crowd boxes are flagged, and
  clipped or tiny balls are turned into ignore regions on the GPU instead of silently
  becoming hard negatives.
- Training pipelines crop to the model input *first*, then run affine/photometric
  augmentation at 120 px instead of on the full 640 px image.
"""

from __future__ import annotations

import json
import math
import random
import zlib
from pathlib import Path
from typing import Any

import cv2
import numpy as np
import torch
from torch.utils.data import DataLoader, Dataset

try:
    import albumentations as A
except ImportError:  # pragma: no cover - albumentations is a hard requirement for training
    A = None

# COCO categories that look like balls or clutter around them; used for hard negatives.
CONFUSER_CATS = {
    1: "person", 34: "frisbee", 38: "kite", 47: "cup", 51: "bowl", 53: "apple",
    55: "orange", 60: "donut", 62: "chair", 85: "clock",
}
# Indoor scenes: a stand-in for the floors and rooms the car drives through.
INDOOR_CATS = {62, 63, 64, 65, 67, 70, 72, 73, 78, 79, 81, 82, 84}
SPLIT_DIRS = ("train2017", "val2017")
INDEX_VERSION = 2
BOX_FIELDS = 6  # cls, x, y, w, h, flag (flag: 0 pad, 1 normal, 2 crowd)


# ────────────────────────────────────────────────────────────────────────────
# Index
# ────────────────────────────────────────────────────────────────────────────

def load_index(coco_root: str | Path, class_ids: list[int], cache_dir: str | Path = "cache",
               verbose: bool = True) -> dict:
    """Images with target annotations, plus a list of target-free images per COCO split.

    Returns {"classes": [...], "images": {id: {"file", "w", "h", "split", "anns"}},
             "neg": {"train2017": [ids], "val2017": [ids]},
             "confuser": {"train2017": [ids], "val2017": [ids]}}
    ``anns`` rows are [x, y, w, h, coco_cat_id, iscrowd, polygons] in original pixels
    (polygons only for non-crowd boxes; used by the copy-paste cutout bank).
    """
    coco_root = Path(coco_root).expanduser()
    cache_dir = Path(cache_dir)
    cache_dir.mkdir(parents=True, exist_ok=True)
    tag = "-".join(str(c) for c in sorted(class_ids))
    path = cache_dir / f"coco_index_v{INDEX_VERSION}_{tag}.json"
    if path.exists():
        index = json.loads(path.read_text())
        index["images"] = {int(k): v for k, v in index["images"].items()}
        if verbose:
            _print_index(index)
        return index

    targets = set(int(c) for c in class_ids)
    images, neg, confuser, indoor = {}, {}, {}, {}
    for split in SPLIT_DIRS:
        ann_path = coco_root / "annotations" / f"instances_{split}.json"
        if verbose:
            print(f"reading {ann_path} (first run only)...")
        raw = json.loads(ann_path.read_text())
        info = {im["id"]: im for im in raw["images"]}
        target_anns: dict[int, list] = {}
        conf_ids, indoor_ids = set(), set()
        for a in raw["annotations"]:
            cat = a["category_id"]
            if cat in targets:
                x, y, w, h = a["bbox"]
                if w > 0 and h > 0:
                    crowd = int(a.get("iscrowd", 0))
                    seg = a.get("segmentation") if not crowd and isinstance(a.get("segmentation"), list) else []
                    seg = [[round(v, 1) for v in poly] for poly in seg]
                    target_anns.setdefault(a["image_id"], []).append([x, y, w, h, cat, crowd, seg])
            else:
                if cat in CONFUSER_CATS:
                    conf_ids.add(a["image_id"])
                if cat in INDOOR_CATS:
                    indoor_ids.add(a["image_id"])
        for img_id, anns in target_anns.items():
            im = info[img_id]
            images[img_id] = {"file": f"{split}/{im['file_name']}", "w": im["width"],
                              "h": im["height"], "split": split, "anns": anns}
        neg_ids = sorted(set(info) - set(target_anns))
        neg[split] = neg_ids
        confuser[split] = sorted(conf_ids - set(target_anns))
        indoor[split] = sorted(indoor_ids - set(target_anns))
        # Target-free images also need file names and sizes.
        for img_id in neg_ids:
            im = info[img_id]
            images[img_id] = {"file": f"{split}/{im['file_name']}", "w": im["width"],
                              "h": im["height"], "split": split, "anns": []}
    index = {"version": INDEX_VERSION, "classes": sorted(targets), "images": images, "neg": neg,
             "confuser": confuser, "indoor": indoor}
    path.write_text(json.dumps(index))
    if verbose:
        _print_index(index)
    return index


def _print_index(index: dict) -> None:
    pos = [i for i, m in index["images"].items() if m["anns"]]
    n_anns = sum(len(index["images"][i]["anns"]) for i in pos)
    n_crowd = sum(a[5] for i in pos for a in index["images"][i]["anns"])
    print(f"index: {len(pos)} images with targets ({n_anns} boxes, {n_crowd} crowd), "
          f"{sum(len(v) for v in index['neg'].values())} target-free images")


# ────────────────────────────────────────────────────────────────────────────
# Fixed splits
# ────────────────────────────────────────────────────────────────────────────

SPLIT_DEFAULTS = {
    "seed": 1234,
    "val_frac": 0.2,            # share of train2017 positives moved into validation
    "val_full_bg_ratio": 0.4,   # val_full keeps the old 40% background mix
    "val_neg_n": 1000,          # confuser-rich target-free images for FP/image
    "val_zoom_per_box": 2,      # zoomed windows per scorable validation box
    "val_zoom_px": [6, 40],     # target diameter at the 120 px reference input
    "val_zoom_ref_input": 120,  # windows are fixed in source pixels (= fixed scene FOV)
    "val_zoom_min_src_px": 2.0, # skip boxes smaller than this in the source image
}


def build_splits(index: dict, cache_dir: str | Path = "cache", **overrides) -> dict:
    """Build (or load) the fixed train/validation splits. Same inputs -> same file."""
    p = {**SPLIT_DEFAULTS, **overrides}
    tag = json.dumps({**p, "classes": index["classes"]}, sort_keys=True)
    import hashlib
    h = hashlib.sha1(tag.encode()).hexdigest()[:8]
    path = Path(cache_dir) / f"splits_{h}.json"
    if path.exists():
        splits = json.loads(path.read_text())
        _print_splits(splits, path)
        return splits

    rng = random.Random(p["seed"])
    imgs = index["images"]
    pos_train = sorted(i for i, m in imgs.items() if m["anns"] and m["split"] == "train2017")
    pos_val = sorted(i for i, m in imgs.items() if m["anns"] and m["split"] == "val2017")
    rng.shuffle(pos_train)
    n_hold = int(round(len(pos_train) * p["val_frac"]))
    val_pos = sorted(pos_val + pos_train[:n_hold])
    train_pos = sorted(pos_train[n_hold:])

    # Validation negatives come from val2017 so they never overlap the training pools.
    val_confusers = list(index["confuser"]["val2017"])
    rng.shuffle(val_confusers)
    val_neg = sorted(val_confusers[:p["val_neg_n"]])
    val_bg_pool = [i for i in index["neg"]["val2017"] if i not in set(val_neg)]
    rng.shuffle(val_bg_pool)
    n_val_bg = int(round(len(val_pos) * p["val_full_bg_ratio"] / (1 - p["val_full_bg_ratio"])))
    val_full = sorted(val_pos + val_bg_pool[:n_val_bg])

    bg_random = list(index["neg"]["train2017"])
    rng.shuffle(bg_random)
    bg_confuser = list(index["confuser"]["train2017"])
    rng.shuffle(bg_confuser)

    val_zoom = _zoom_windows(imgs, val_pos, p, rng)
    splits = {"params": p, "train_pos": train_pos, "val_pos": val_pos, "val_full": val_full,
              "val_neg": val_neg, "val_zoom": val_zoom,
              "bg_random": bg_random, "bg_confuser": bg_confuser}
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(splits))
    _print_splits(splits, path)
    return splits


def _zoom_windows(imgs: dict, ids: list[int], p: dict, rng: random.Random) -> list[list]:
    """Deterministic square windows [img_id, x0, y0, win] around validation boxes."""
    out = []
    ref = p["val_zoom_ref_input"]
    for img_id in ids:
        m = imgs[img_id]
        W, H = m["w"], m["h"]
        for x, y, w, h, _cat, crowd, *_ in m["anns"]:
            if crowd or min(w, h) < p["val_zoom_min_src_px"]:
                continue
            for _ in range(p["val_zoom_per_box"]):
                target = rng.uniform(*p["val_zoom_px"])
                win = max(min(w, h), 1.0) * ref / target
                win = int(min(max(win, ref / 2), W, H))
                x0 = _place(x + w / 2, W, win, rng)
                y0 = _place(y + h / 2, H, win, rng)
                out.append([img_id, x0, y0, win])
    return out


def _place(c: float, size: int, win: int, rng: random.Random) -> int:
    """Window origin so the centre ``c`` lands in the middle 80% of the window."""
    lo, hi = max(0.0, c - 0.9 * win), min(size - win, c - 0.1 * win)
    if lo <= hi:
        return int(rng.uniform(lo, hi))
    return int(min(max(c - win / 2, 0), size - win))


def _print_splits(s: dict, path: Path) -> None:
    print(f"splits ({path.name}): train_pos={len(s['train_pos'])} val_pos={len(s['val_pos'])} "
          f"val_full={len(s['val_full'])} val_zoom={len(s['val_zoom'])} windows "
          f"val_neg={len(s['val_neg'])} | bg pools random={len(s['bg_random'])} "
          f"confuser={len(s['bg_confuser'])}")


def _indoor_pool(splits: dict, index: dict | None) -> list[int]:
    if "bg_indoor" not in splits:
        indoor = set(index["indoor"]["train2017"]) if index and "indoor" in index else set()
        splits["bg_indoor"] = [i for i in splits["bg_random"] if i in indoor]  # keeps the shuffled order
    return splits["bg_indoor"]


def training_ids(splits: dict, bg_ratio: float, bg_source: str, index: dict | None = None) -> tuple[list[int], list[int]]:
    """Positive and background image ids for one data configuration."""
    pos = list(splits["train_pos"])
    n_bg = int(round(len(pos) * bg_ratio / max(1e-6, 1 - bg_ratio)))
    if bg_source == "random":
        bg = splits["bg_random"][:n_bg]
    elif bg_source == "confusers":
        bg = splits["bg_confuser"][:n_bg]
    elif bg_source == "indoor":
        bg = _indoor_pool(splits, index)[:n_bg]
    elif bg_source == "mixed":
        half = n_bg // 2
        conf = splits["bg_confuser"][:half]
        taken = set(conf)
        rand = [i for i in splits["bg_random"] if i not in taken][: n_bg - half]
        bg = conf + rand
    else:
        raise ValueError(f"unknown data.bg_source {bg_source!r}")
    return pos, list(bg)


# ────────────────────────────────────────────────────────────────────────────
# Augmentation
# ────────────────────────────────────────────────────────────────────────────

def _bbox_params():
    kw = dict(format="coco", label_fields=["labels", "flags"], min_visibility=0.0, min_area=0.0)
    try:
        return A.BboxParams(clip=True, **kw)
    except TypeError:  # older albumentations without `clip`
        return A.BboxParams(**kw)


def _pad(size: int):
    try:
        return A.PadIfNeeded(min_height=size, min_width=size, border_mode=cv2.BORDER_CONSTANT, fill=0)
    except TypeError:
        return A.PadIfNeeded(min_height=size, min_width=size, border_mode=cv2.BORDER_CONSTANT, value=0)


def _affine():
    try:
        return A.Affine(scale=(0.9, 1.1), rotate=(-20, 20), translate_percent=(0, 0.08),
                        border_mode=cv2.BORDER_CONSTANT, fill=0, p=0.5)
    except TypeError:
        return A.Affine(scale=(0.9, 1.1), rotate=(-20, 20), translate_percent=(0, 0.08), p=0.5)


def photometric(preset: str) -> list:
    """Generic photometric augmentation (pixel-only). The camera model is separate (camera.py)."""
    if preset == "device":  # legacy name: the camera simulation is now data.device_strength
        preset = "light"
    p = {"light": 0.2, "strong": 0.5}[preset]
    ops = [
        A.ImageCompression(quality_range=(30, 75), p=p),
        A.Downscale(scale_range=(0.5, 0.9), p=p),
        A.RandomBrightnessContrast(brightness_limit=0.3, contrast_limit=0.3, p=p),
        A.ISONoise(color_shift=(0.01, 0.05), intensity=(0.1, 0.5), p=p),
        A.MotionBlur(blur_limit=3, p=p),
        A.ColorJitter(brightness=0.2, contrast=0.2, saturation=0.2, hue=0.05, p=p),
    ]
    if preset == "strong":
        ops += [A.RandomGamma(gamma_limit=(70, 140), p=0.3)]
    return ops


def build_transforms(input_size: int, aug: str, nearest: bool = False, device_geo: list | None = None) -> dict:
    """Albumentations pipelines keyed by role.

    train / zoom: geometric only (crop to the input, flip, affine), boxes follow.
    camera_geo:   lens distortion + rolling-shutter shear (applied after copy-paste).
    photo:        generic photometric ops, pixel-only.
    ``nearest`` mimics the camera DSP's unfiltered downscaling.
    """
    s = input_size
    interp = cv2.INTER_NEAREST if nearest else cv2.INTER_AREA
    bp = _bbox_params()
    train = A.Compose([
        A.OneOf([
            A.RandomSizedBBoxSafeCrop(height=s, width=s, interpolation=interp, p=1.0),
            A.RandomResizedCrop(size=(s, s), scale=(0.4, 1.0), ratio=(0.8, 1.25), interpolation=interp, p=1.0),
            A.Sequential([A.LongestMaxSize(max_size=s, interpolation=interp), _pad(s)], p=1.0),
        ], p=1.0),
        A.HorizontalFlip(p=0.5),
        _affine(),
    ], bbox_params=bp)
    zoom = A.Compose([A.Resize(s, s, interpolation=interp), A.HorizontalFlip(p=0.5), _affine()], bbox_params=bp)
    val = A.Compose([A.LongestMaxSize(max_size=s, interpolation=cv2.INTER_AREA), _pad(s)], bbox_params=bp)
    window = A.Compose([A.Resize(s, s, interpolation=cv2.INTER_AREA)], bbox_params=bp)
    out = {"train": train, "zoom": zoom, "val": val, "window": window,
           "photo": A.Compose(photometric(aug)) if aug else None,
           "camera_geo": A.Compose(device_geo, bbox_params=bp) if device_geo else None}
    return out


# Fixed camera model for the val_dev split (a proxy for the real camera until real frames exist)
DEV_EVAL_SIM = {"strength": 0.6, "motion": 0.3, "lens": "none", "seed": 777}


# ────────────────────────────────────────────────────────────────────────────
# Dataset
# ────────────────────────────────────────────────────────────────────────────

class DetectionDataset(Dataset):
    """Items are (image_id, window) where window is None or (x0, y0, win) in source pixels.

    ``__getitem__`` returns (uint8 tensor [3, S, S], float tensor [n, 6]) with rows
    (class_index, x, y, w, h, flag) in input pixels.

    Training order: geometric crop -> copy-paste -> camera geometry (lens, rolling shutter)
    -> generic photometric -> camera sensor/ISP/RGB565 (camera.py).
    ``eval_sim`` applies a seeded, per-item deterministic camera model (val_dev).
    """

    def __init__(self, index: dict, items: list, coco_root: str | Path, class_ids: list[int],
                 input_size: int, train: bool, aug: str = "light", zoom_prob: float = 0.0,
                 zoom_px=(6, 32), device: dict | None = None, paste: dict | None = None,
                 bank=None, eval_sim: dict | None = None):
        self.images = index["images"]
        self.items = [(int(i[0]), tuple(i[1:]) if len(i) > 1 else None) if isinstance(i, (list, tuple))
                      else (int(i), None) for i in items]
        self.root = Path(coco_root).expanduser()
        self.cls_map = {int(c): k for k, c in enumerate(class_ids)}
        self.size = input_size
        self.train = train
        self.aug = aug
        self.zoom_prob = zoom_prob
        self.zoom_px = tuple(zoom_px)
        self.device = device or {"strength": 0.0, "motion": 0.0, "lens": "none"}
        self.paste = paste or {"prob": 0.0}
        self.bank = bank
        self.eval_sim = eval_sim
        self._tf = None
        self._tf_nearest = None
        self._sim = None

    def __len__(self) -> int:
        return len(self.items)

    @property
    def device_on(self) -> bool:
        return self.train and (self.device["strength"] > 0 or self.device["motion"] > 0 or self.device["lens"] != "none")

    def _transforms(self):
        if self._tf is None:  # built lazily inside each worker process
            from . import camera
            geo = camera.geometric(self.device["strength"], self.device["motion"], self.device["lens"]) \
                if self.device_on else None
            self._tf = build_transforms(self.size, self.aug if self.train else None, device_geo=geo)
            if self.device_on and self.device["strength"] > 0:
                self._tf_nearest = build_transforms(self.size, self.aug, nearest=True, device_geo=geo)
            if self.device_on:
                self._sim = camera.CameraSim(self.device["strength"], self.device["motion"], self.device["lens"])
        if self._tf_nearest is not None and random.random() < 0.5 * self.device["strength"]:
            return self._tf_nearest
        return self._tf

    def load(self, img_id: int):
        """RGB image and its target boxes (scaled to the stored image's size)."""
        m = self.images[img_id]
        img = cv2.imread(str(self.root / m["file"]), cv2.IMREAD_COLOR)
        if img is None:
            raise FileNotFoundError(self.root / m["file"])
        img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
        H, W = img.shape[:2]
        sx, sy = W / m["w"], H / m["h"]
        boxes, labels, flags = [], [], []
        for x, y, w, h, cat, crowd, *_ in m["anns"]:
            if cat not in self.cls_map:
                continue
            bx, by = max(0.0, x * sx), max(0.0, y * sy)
            bw, bh = min(w * sx, W - bx), min(h * sy, H - by)
            if bw < 1e-3 or bh < 1e-3:
                continue
            boxes.append([bx, by, bw, bh])
            labels.append(self.cls_map[cat])
            flags.append(2 if crowd else 1)
        return img, boxes, labels, flags

    def __getitem__(self, i: int):
        img_id, window = self.items[i]
        img, boxes, labels, flags = self.load(img_id)
        tf = self._transforms()
        if window is not None:
            m = self.images[img_id]
            s = img.shape[1] / m["w"]
            x0, y0, win = (int(round(v * s)) for v in window)
            img, boxes, labels, flags = crop(img, boxes, labels, flags, x0, y0, win)
            pipe = tf["zoom"] if self.train else tf["window"]
        elif self.train and self.zoom_prob > 0 and random.random() < self.zoom_prob:
            z = self._random_zoom(img, boxes, labels, flags)
            if z is not None:
                img, boxes, labels, flags = z
                pipe = tf["zoom"]
            else:
                pipe = tf["train"]
        else:
            pipe = tf["train"] if self.train else tf["val"]

        out = pipe(image=img, bboxes=boxes, labels=labels, flags=flags)
        img = out["image"]
        boxes, labels, flags = _clean(out["bboxes"], out["labels"], out["flags"], img.shape[1], img.shape[0])
        if self.train:
            rng = np.random.default_rng(random.getrandbits(32))
            if self.bank is not None and self.paste.get("prob", 0) > 0 and random.random() < self.paste["prob"]:
                from .paste import paste
                img, boxes, labels, flags = paste(img, boxes, labels, flags, self.bank, self.cls_map,
                                                  self.paste.get("max", 2), self.paste.get("px", (6, 36)),
                                                  self.paste.get("floor_bias", 0.5), rng)
            if tf["camera_geo"] is not None:
                boxes, labels, flags = _clean(boxes, labels, flags, img.shape[1], img.shape[0])
                o = tf["camera_geo"](image=img, bboxes=boxes, labels=labels, flags=flags)
                img = o["image"]
                boxes, labels, flags = _clean(o["bboxes"], o["labels"], o["flags"], img.shape[1], img.shape[0])
            if tf["photo"] is not None:
                img = tf["photo"](image=img)["image"]
            if self._sim is not None:
                img = self._sim(img, rng)
        elif self.eval_sim is not None:
            from . import camera
            if self._sim is None:
                self._sim = camera.CameraSim(self.eval_sim["strength"], self.eval_sim["motion"], self.eval_sim["lens"])
            img = self._sim(img, np.random.default_rng(self.eval_sim["seed"] * 100003 + i))

        im = torch.from_numpy(np.ascontiguousarray(img.transpose(2, 0, 1)))
        rows = [[l, *b, f] for b, l, f in zip(boxes, labels, flags)]
        bx = torch.tensor(rows, dtype=torch.float32) if rows else torch.zeros((0, BOX_FIELDS))
        return im, bx

    def _random_zoom(self, img, boxes, labels, flags):
        cand = [k for k, f in enumerate(flags) if f == 1]
        if not cand:
            return None
        x, y, w, h = boxes[random.choice(cand)]
        H, W = img.shape[:2]
        win = max(min(w, h), 1.0) * self.size / random.uniform(*self.zoom_px)
        win = int(min(max(win, self.size / 2), W, H))  # never upscale more than 2x
        x0 = _place(x + w / 2, W, win, random)
        y0 = _place(y + h / 2, H, win, random)
        return crop(img, boxes, labels, flags, x0, y0, win)


def _clean(boxes, labels, flags, W, H, min_side: float = 0.5):
    """Clip boxes to the image and drop slivers, so the next Albumentations stage accepts them."""
    nb, nl, nf = [], [], []
    for (x, y, w, h), l, f in zip(boxes, labels, flags):
        x0, y0 = max(0.0, float(x)), max(0.0, float(y))
        x1, y1 = min(float(W), float(x) + float(w)), min(float(H), float(y) + float(h))
        if x1 - x0 >= min_side and y1 - y0 >= min_side:
            nb.append([x0, y0, x1 - x0, y1 - y0])
            nl.append(l)
            nf.append(f)
    return nb, nl, nf


def crop(img, boxes, labels, flags, x0, y0, win):
    """Square crop; boxes are intersected with it (clipped ones are handled later)."""
    c = img[y0:y0 + win, x0:x0 + win]
    nb, nl, nf = [], [], []
    for (bx, by, bw, bh), l, f in zip(boxes, labels, flags):
        cx0, cy0 = max(bx, x0), max(by, y0)
        cx1, cy1 = min(bx + bw, x0 + win), min(by + bh, y0 + win)
        if cx1 - cx0 < 1e-3 or cy1 - cy0 < 1e-3:
            continue
        nb.append([cx0 - x0, cy0 - y0, cx1 - cx0, cy1 - cy0])
        nl.append(l)
        nf.append(f)
    return c, nb, nl, nf


def collate(batch):
    imgs = torch.stack([b[0] for b in batch])
    m = max(1, max(b[1].shape[0] for b in batch))
    boxes = torch.zeros((len(batch), m, BOX_FIELDS))
    for k, (_, bx) in enumerate(batch):
        if bx.shape[0]:
            boxes[k, : bx.shape[0]] = bx
    return imgs, boxes


def _worker_init(worker_id: int):
    seed = torch.initial_seed() % 2**32
    np.random.seed(seed)
    random.seed(seed)


def make_loader(ds: Dataset, batch_size: int, shuffle: bool, workers: int, seed: int = 0,
                drop_last: bool = False, persistent: bool = True) -> DataLoader:
    g = torch.Generator()
    g.manual_seed(seed)
    return DataLoader(ds, batch_size=batch_size, shuffle=shuffle, num_workers=workers,
                      collate_fn=collate, pin_memory=torch.cuda.is_available(),
                      drop_last=drop_last, worker_init_fn=_worker_init, generator=g,
                      persistent_workers=persistent and workers > 0,
                      prefetch_factor=(4 if persistent else 2) if workers > 0 else None)


# ────────────────────────────────────────────────────────────────────────────
# Builders used by the trainer and notebook
# ────────────────────────────────────────────────────────────────────────────

def register_extra_backgrounds(index: dict, folder: str | Path) -> list[int]:
    """Add every image in ``folder`` (e.g. ball-free frames from the car) as a background.

    They get negative ids so they never collide with COCO ids. Returns their ids.
    """
    folder = Path(folder).expanduser()
    ids = []
    files = sorted(p for p in folder.rglob("*") if p.suffix.lower() in (".jpg", ".jpeg", ".png", ".bmp"))
    for k, f in enumerate(files):
        img_id = -(k + 1) - 10_000_000 * (zlib.crc32(str(folder).encode()) % 100)
        if img_id not in index["images"]:
            im = cv2.imread(str(f))
            if im is None:
                continue
            index["images"][img_id] = {"file": str(f.resolve()), "w": im.shape[1], "h": im.shape[0],
                                       "split": "extra", "anns": []}
        ids.append(img_id)
    return ids


def train_dataset(cfg, index: dict, splits: dict, bank=None) -> DetectionDataset:
    pos, bg = training_ids(splits, cfg["data.bg_ratio"], cfg["data.bg_source"], index)
    if cfg["data.extra_bg_dir"]:
        bg += register_extra_backgrounds(index, cfg["data.extra_bg_dir"]) * int(cfg["data.extra_bg_repeat"])
    device = {"strength": cfg["data.device_strength"], "motion": cfg["data.device_motion"],
              "lens": cfg["data.device_lens"]}
    paste = {"prob": cfg["data.paste_prob"], "max": cfg["data.paste_max"], "px": cfg["data.paste_px"],
             "floor_bias": cfg["data.paste_floor_bias"]}
    if paste["prob"] > 0 and bank is None:
        raise ValueError("data.paste_prob > 0 needs a cutout bank (DataContext builds it)")
    return DetectionDataset(index, pos + bg, cfg["data.coco_root"], cfg.class_ids,
                            cfg["data.input_size"], train=True, aug=cfg["data.aug"],
                            zoom_prob=cfg["data.zoom_prob"], zoom_px=cfg["data.zoom_px"],
                            device=device, paste=paste, bank=bank)


EVAL_SPLITS = ("val_zoom", "val_dev", "val_full", "val_neg")


def eval_dataset(name: str, cfg, index: dict, splits: dict) -> DetectionDataset:
    """val_dev = every other val_zoom window through a fixed, seeded camera model.
    val_zoom_quick = a fixed 768-window subset of val_zoom for learning curves."""
    if name == "val_zoom_quick":
        return DetectionDataset(index, splits["val_zoom"][1::3][:768], cfg["data.coco_root"], cfg.class_ids,
                                cfg["data.input_size"], train=False)
    if name == "val_dev":
        return DetectionDataset(index, splits["val_zoom"][::2], cfg["data.coco_root"], cfg.class_ids,
                                cfg["data.input_size"], train=False, eval_sim=DEV_EVAL_SIM)
    return DetectionDataset(index, splits[name], cfg["data.coco_root"], cfg.class_ids,
                            cfg["data.input_size"], train=False)


def calibration_items(splits: dict, n: int = 400, pos_frac: float = 0.8, seed: int = 0) -> list:
    """Shuffled, positive-heavy calibration set: zoom windows + full positives + negatives."""
    rng = random.Random(seed)
    n_pos = int(n * pos_frac)
    zoom = list(splits["val_zoom"])
    full = [[i] for i in splits["val_pos"]]
    neg = [[i] for i in splits["val_neg"]]
    rng.shuffle(zoom), rng.shuffle(full), rng.shuffle(neg)
    items = zoom[: n_pos // 2] + full[: n_pos - n_pos // 2] + neg[: n - n_pos]
    rng.shuffle(items)
    return items


def profile_loader(loader: DataLoader, n_batches: int = 30) -> float:
    """Images per second the loader delivers (to tell data-bound from GPU-bound)."""
    import time
    it = iter(loader)
    next(it)  # warm up workers
    t, n = time.time(), 0
    for _ in range(n_batches):
        try:
            imgs, _ = next(it)
        except StopIteration:
            break
        n += imgs.shape[0]
    rate = n / max(1e-6, time.time() - t)
    print(f"loader: {rate:.0f} images/s")
    return rate
