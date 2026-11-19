"""Copy-paste augmentation: crisp ball cutouts from COCO segmentation masks.

Zooming a 5 px COCO ball to 20 px gives a blurry blob. Instead, balls that are large in
their source image (min side >= ``min_px``, not crowd) are cut out once with their
polygon masks and pasted, *downscaled*, onto training images at the sizes the car sees.

Only training-split images feed the bank, so validation balls never leak into training.

Pasting happens after the geometric crop and before the camera simulation, so pasted
balls get the same lens, blur, noise and RGB565 treatment as real ones. Realism touches:
- the alpha edge is feathered by ~1 px at output scale;
- the cutout's brightness is pulled part-way toward its local background;
- with ``floor_bias`` > 0, positions favour the lower part of the frame (the floor, from a
  camera a few cm high) and a soft contact shadow is drawn under the ball.
"""

from __future__ import annotations

import hashlib
import json
import math
from pathlib import Path

import cv2
import numpy as np


class CutoutBank:
    def __init__(self, folder: str | Path):
        self.dir = Path(folder)
        self.items = json.loads((self.dir / "manifest.json").read_text())
        self._cache: dict[int, np.ndarray] = {}

    def __len__(self) -> int:
        return len(self.items)

    def get(self, i: int) -> tuple[np.ndarray, int]:
        """(RGBA uint8 cutout, COCO category id)."""
        if i not in self._cache:
            img = cv2.imread(str(self.dir / self.items[i]["file"]), cv2.IMREAD_UNCHANGED)
            self._cache[i] = cv2.cvtColor(img, cv2.COLOR_BGRA2RGBA)
        return self._cache[i], self.items[i]["cat"]

    @classmethod
    def build(cls, index: dict, splits: dict, coco_root: str | Path, cache_dir: str | Path = "cache",
              min_px: float = 20, max_items: int = 5000, verbose: bool = True) -> "CutoutBank":
        tag = hashlib.sha1(json.dumps([2, index["classes"], min_px, max_items, len(splits["train_pos"])]).encode()).hexdigest()[:8]
        folder = Path(cache_dir) / f"cutouts_{tag}"
        if (folder / "manifest.json").exists():
            bank = cls(folder)
            if verbose:
                print(f"cutout bank: {len(bank)} cutouts ({folder})")
            return bank
        folder.mkdir(parents=True, exist_ok=True)
        root = Path(coco_root).expanduser()
        items = []
        for img_id in splits["train_pos"]:
            m = index["images"][img_id]
            cands = [a for a in m["anns"] if not a[5] and min(a[2], a[3]) >= min_px and len(a) > 6 and a[6]]
            if not cands:
                continue
            img = cv2.imread(str(root / m["file"]), cv2.IMREAD_COLOR)
            if img is None:
                continue
            H, W = img.shape[:2]
            sx, sy = W / m["w"], H / m["h"]
            for x, y, w, h, cat, _crowd, seg in cands:
                # balls are round: skip ones clipped by the image edge or squashed boxes
                if min(w, h) / max(w, h) < 0.75 or x <= 1 or y <= 1 or x + w >= m["w"] - 1 or y + h >= m["h"] - 1:
                    continue
                mask = np.zeros((H, W), np.uint8)
                for poly in seg:
                    pts = (np.asarray(poly, np.float32).reshape(-1, 2) * [sx, sy]).astype(np.int32)
                    cv2.fillPoly(mask, [pts], 255)
                x0, y0 = max(0, int(x * sx) - 1), max(0, int(y * sy) - 1)
                x1, y1 = min(W, int(math.ceil((x + w) * sx)) + 1), min(H, int(math.ceil((y + h) * sy)) + 1)
                if mask[y0:y1, x0:x1].mean() < 0.4 * 255:  # mask covers too little of its box
                    continue
                rgba = np.dstack([img[y0:y1, x0:x1], mask[y0:y1, x0:x1]])
                name = f"{len(items):05d}.png"
                cv2.imwrite(str(folder / name), rgba)
                items.append({"file": name, "cat": int(cat), "src": int(img_id), "w": x1 - x0, "h": y1 - y0})
                if len(items) >= max_items:
                    break
            if len(items) >= max_items:
                break
        (folder / "manifest.json").write_text(json.dumps(items))
        if verbose:
            print(f"cutout bank: built {len(items)} cutouts from training images ({folder})")
        return cls(folder)


def paste(img: np.ndarray, boxes: list, labels: list, flags: list, bank: CutoutBank, cls_map: dict,
          n_max: int, px_range, floor_bias: float, rng: np.random.Generator):
    """Paste 1..n_max cutouts onto an RGB uint8 image (already at model input size)."""
    if len(bank) == 0:
        return img, boxes, labels, flags
    S_h, S_w = img.shape[:2]
    out = img.astype(np.float32)
    boxes, labels, flags = list(boxes), list(labels), list(flags)
    for _ in range(int(rng.integers(1, n_max + 1))):
        cut, cat = bank.get(int(rng.integers(len(bank))))
        if cat not in cls_map:
            continue
        d = math.exp(rng.uniform(math.log(px_range[0]), math.log(px_range[1])))
        scale = d / max(1.0, min(cut.shape[0], cut.shape[1]))
        cw, ch = max(2, int(round(cut.shape[1] * scale))), max(2, int(round(cut.shape[0] * scale)))
        if cw >= S_w or ch >= S_h:
            continue
        c = cv2.resize(cut, (cw, ch), interpolation=cv2.INTER_AREA if scale < 1 else cv2.INTER_LINEAR)
        if rng.random() < 0.5:
            c = c[:, ::-1]
        k = int(rng.integers(4))
        c = np.ascontiguousarray(np.rot90(c, k))
        ch, cw = c.shape[:2]
        # position: uniform in x; y skewed toward the bottom of the frame by floor_bias
        for _try in range(10):
            px = int(rng.integers(0, S_w - cw + 1))
            yf = rng.random() ** (1.0 / (1.0 + 2.0 * floor_bias))
            py = int(round(yf * (S_h - ch)))
            if not any(px < bx + bw and px + cw > bx and py < by + bh and py + ch > by
                       for (bx, by, bw, bh) in boxes):
                break
        else:
            continue
        a = c[..., 3].astype(np.float32) / 255.0
        a = cv2.GaussianBlur(a, (0, 0), rng.uniform(0.4, 0.9))  # feathered edge
        rgb = c[..., :3].astype(np.float32)
        region = out[py:py + ch, px:px + cw]
        # pull brightness part-way toward the local background
        bg_l = region.mean()
        fg_l = (rgb * a[..., None]).sum() / max(1e-3, a.sum() * 3)
        g = float(np.clip((bg_l / max(1.0, fg_l)) ** rng.uniform(0.0, 0.5), 0.6, 1.6))
        rgb = np.clip(rgb * g, 0, 255)
        # soft contact shadow on the floor under the ball
        if floor_bias > 0 and rng.random() < 0.7:
            sh = np.zeros((S_h, S_w), np.float32)
            cx, cy = px + cw / 2, py + ch * 0.95
            cv2.ellipse(sh, (int(cx), int(cy)), (max(1, int(cw * 0.55)), max(1, int(ch * 0.18))), 0, 0, 360, 1.0, -1)
            sh = cv2.GaussianBlur(sh, (0, 0), max(0.6, 0.15 * cw))
            out *= (1.0 - rng.uniform(0.15, 0.45) * sh)[..., None]
            region = out[py:py + ch, px:px + cw]
        out[py:py + ch, px:px + cw] = region * (1 - a[..., None]) + rgb * a[..., None]
        ys, xs = np.nonzero(a > 0.5)
        if len(xs) == 0:
            continue
        boxes.append([float(px + xs.min()), float(py + ys.min()), float(xs.max() - xs.min() + 1),
                      float(ys.max() - ys.min() + 1)])
        labels.append(cls_map[cat])
        flags.append(1)
    return np.clip(out, 0, 255).astype(np.uint8), boxes, labels, flags
