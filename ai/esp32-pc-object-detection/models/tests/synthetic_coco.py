"""A small synthetic COCO-format dataset for testing tinydet without the real COCO.

Textured backgrounds with coloured circles annotated as `sports ball` (with polygon masks,
a few crowd boxes) and orange squares annotated as `orange` (confusers).
"""

import json
import random
from pathlib import Path

import cv2
import numpy as np


def make(root: str | Path, n_train: int = 360, n_val: int = 160, seed: int = 0) -> Path:
    root = Path(root)
    if (root / "annotations" / "instances_val2017.json").exists():
        return root
    rng = random.Random(seed)
    nrng = np.random.default_rng(seed)
    cats = [{"id": 1, "name": "person"}, {"id": 37, "name": "sports ball"}, {"id": 55, "name": "orange"}]
    ann_id = 1
    for split, n in (("train2017", n_train), ("val2017", n_val)):
        (root / split).mkdir(parents=True, exist_ok=True)
        images, anns = [], []
        for i in range(n):
            img_id = (1 if split == "train2017" else 100000) + i
            W, H = rng.choice([(320, 240), (240, 320), (300, 300)])
            img = (nrng.random((H, W, 3)) * 60 + rng.randint(40, 160)).astype(np.uint8)
            img = cv2.GaussianBlur(img, (7, 7), 0)
            kind = rng.random()
            if kind < 0.55:
                for _ in range(rng.randint(1, 3)):
                    r = rng.choice([1, 2, 3, 5, 8, 12, 20, 30])
                    cx, cy = rng.randint(0, W - 1), rng.randint(0, H - 1)
                    col = (rng.randint(150, 255), rng.randint(150, 255), rng.randint(0, 80))
                    cv2.circle(img, (cx, cy), r, col, -1)
                    x0, y0 = max(0, cx - r), max(0, cy - r)
                    x1, y1 = min(W, cx + r), min(H, cy + r)
                    poly = [float(v) for t in np.linspace(0, 2 * np.pi, 16, endpoint=False)
                            for v in (cx + r * np.cos(t), cy + r * np.sin(t))]
                    anns.append({"id": ann_id, "image_id": img_id, "category_id": 37,
                                 "bbox": [x0, y0, x1 - x0, y1 - y0], "area": (x1 - x0) * (y1 - y0),
                                 "iscrowd": 1 if rng.random() < 0.03 else 0, "segmentation": [poly]})
                    ann_id += 1
            elif kind < 0.75:
                x, y = rng.randint(0, W - 30), rng.randint(0, H - 30)
                cv2.rectangle(img, (x, y), (x + 25, y + 25), (255, 140, 0), -1)
                anns.append({"id": ann_id, "image_id": img_id, "category_id": 55, "bbox": [x, y, 25, 25],
                             "area": 625, "iscrowd": 0, "segmentation": [[x, y, x + 25, y, x + 25, y + 25, x, y + 25]]})
                ann_id += 1
            name = f"{img_id:012d}.jpg"
            cv2.imwrite(str(root / split / name), cv2.cvtColor(img, cv2.COLOR_RGB2BGR))
            images.append({"id": img_id, "file_name": name, "width": W, "height": H})
        (root / "annotations").mkdir(exist_ok=True)
        (root / "annotations" / f"instances_{split}.json").write_text(
            json.dumps({"images": images, "annotations": anns, "categories": cats}))
    return root
