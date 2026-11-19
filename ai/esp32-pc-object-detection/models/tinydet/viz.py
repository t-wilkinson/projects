"""Notebook previews: augmented samples with their targets, and model predictions."""

from __future__ import annotations

import math

import matplotlib.pyplot as plt
import numpy as np
import torch

from . import evaluate as E
from . import models as M
from . import targets as T


def preview(ds, cfg, n: int = 12, cols: int = 6, seed: int | None = None, heat: bool = True):
    """Augmented samples: positive boxes green, ignored boxes orange, target heatmap overlaid."""
    rng = np.random.default_rng(seed)
    idx = rng.choice(len(ds), size=min(n, len(ds)), replace=False)
    batch = [ds[int(i)] for i in idx]
    imgs = torch.stack([b[0] for b in batch])
    m = max(1, max(b[1].shape[0] for b in batch))
    boxes = torch.zeros(len(batch), m, 6)
    for k, (_, bx) in enumerate(batch):
        boxes[k, : bx.shape[0]] = bx
    t = T.render(boxes, cfg.num_classes, cfg["data.input_size"], cfg["data.cell_size"], T.TargetSpec.from_config(cfg))
    rows = math.ceil(len(batch) / cols)
    fig, axes = plt.subplots(rows, cols, figsize=(cols * 2.6, rows * 2.6))
    s = cfg["data.input_size"]
    for k, ax in enumerate(np.atleast_1d(axes).flat):
        ax.axis("off")
        if k >= len(batch):
            continue
        ax.imshow(imgs[k].permute(1, 2, 0).numpy())
        if heat:
            h = t["heatmap"][k].amax(0).numpy()
            w = t["weight"][k, 0].numpy()
            ax.imshow(h, extent=(0, s, s, 0), cmap="Blues", alpha=0.45 * (h > 0.02), vmin=0, vmax=1)
            ax.imshow(np.where(w == 0, 1.0, np.nan), extent=(0, s, s, 0), cmap="Oranges", alpha=0.35, vmin=0, vmax=1)
        for j in range(m):
            if boxes[k, j, 5] == 0:
                continue
            _, x, y, bw, bh, _ = boxes[k, j].tolist()
            c = "#1baf7a" if t["positive"][k, j] else "#eb6834"
            ax.add_patch(plt.Rectangle((x, y), bw, bh, fill=False, color=c, lw=1.5))
    fig.suptitle("green = positive, orange = ignored (tiny / crowd / clipped); blue = target heatmap", fontsize=10)
    plt.tight_layout()
    return fig


@torch.no_grad()
def predictions(model, cfg, ds, n: int = 12, cols: int = 6, thr: float = 0.3, seed: int | None = 0, device=None):
    """Ground truth (circles) vs detections (crosses with score)."""
    device = device or next(model.parameters()).device
    prep = M.InputPrep(cfg, device)
    rng = np.random.default_rng(seed)
    idx = rng.choice(len(ds), size=min(n, len(ds)), replace=False)
    rows = math.ceil(len(idx) / cols)
    fig, axes = plt.subplots(rows, cols, figsize=(cols * 2.6, rows * 2.6))
    cell = cfg["data.cell_size"]
    model.eval()
    for ax, i in zip(np.atleast_1d(axes).flat, idx):
        img, bx = ds[int(i)]
        logits = model(prep(img[None].to(device))).float()
        dets = E.decode_peaks(torch.sigmoid(logits), thr)[0]
        ax.imshow(img.permute(1, 2, 0).numpy())
        for _, x, y, w, h, f in bx.tolist():
            if f:
                ax.plot(x + w / 2, y + h / 2, "o", mfc="none", mec="#1baf7a", ms=12, mew=2)
        for c, gx, gy, s in dets:
            ax.plot((gx + 0.5) * cell, (gy + 0.5) * cell, "x", color="#e34948", ms=10, mew=2)
            ax.text((gx + 0.5) * cell + 3, (gy + 0.5) * cell - 3, f"{s:.2f}", color="white", fontsize=7,
                    bbox=dict(facecolor="#e34948", alpha=0.7, pad=1, lw=0))
        ax.set_title(f"GT={int((bx[:, 5] > 0).sum())} pred={len(dets)}", fontsize=9)
        ax.axis("off")
    for ax in np.atleast_1d(axes).flat[len(idx):]:
        ax.axis("off")
    plt.tight_layout()
    return fig


def camera_strengths(img, settings=((0, 0), (0.3, 0), (0.6, 0.3), (0.9, 0.6)), lens: str = "mild", seed: int = 0):
    """One image through the camera simulation at several (strength, motion) settings."""
    from .camera import CameraSim
    if isinstance(img, torch.Tensor):
        img = img.permute(1, 2, 0).numpy()
    fig, axes = plt.subplots(1, len(settings), figsize=(3 * len(settings), 3.2))
    for k, (ax, (s, m)) in enumerate(zip(np.atleast_1d(axes), settings)):
        out = img if (s == 0 and m == 0) else CameraSim(s, m, lens)(img, np.random.default_rng(seed + k))
        ax.imshow(out)
        ax.set_title(f"strength {s}, motion {m}", fontsize=9)
        ax.axis("off")
    plt.tight_layout()
    return fig
