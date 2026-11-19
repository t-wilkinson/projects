"""Heatmap losses. All take logits [B, C, G, G] and the dict from ``targets.render``,
compute in float32 (safe under bf16 autocast) and normalise by the number of positives."""

from __future__ import annotations

import torch
import torch.nn.functional as F


def centernet_focal(logits, t, alpha: float = 2.0, beta: float = 4.0):
    """Penalty-reduced pixel-wise focal loss (CornerNet / CenterNet).

    Positive cells: (1-p)^alpha * log p. Elsewhere (1-Y)^beta * p^alpha * log(1-p), so
    cells near a centre are barely punished for firing.
    """
    p = torch.sigmoid(logits.float()).clamp(1e-4, 1 - 1e-4)
    Y, center, w = t["heatmap"], t["center"], t["weight"]
    pos = ((1 - p) ** alpha) * torch.log(p)
    neg = ((1 - Y) ** beta) * (p ** alpha) * torch.log(1 - p)
    loss = -(pos * center + neg * ~center) * w
    return loss.sum() / center.sum().clamp(min=1).float()


def bce(logits, t, pos_weight: float = 50.0):
    """Soft-target BCE; centre cells weighted by ``pos_weight``."""
    x = logits.float()
    Y, center, w = t["heatmap"], t["center"], t["weight"]
    cell_w = w * (1 + (pos_weight - 1) * center.float())
    loss = F.binary_cross_entropy_with_logits(x, Y, reduction="none") * cell_w
    return loss.sum() / center.sum().clamp(min=1).float()


def sigmoid_focal(logits, t, gamma: float = 2.0, alpha_t: float = 0.25):
    """RetinaNet focal loss on hard targets (centre = 1)."""
    x = logits.float()
    Y = t["center"].float()
    p = torch.sigmoid(x)
    ce = F.binary_cross_entropy_with_logits(x, Y, reduction="none")
    p_t = p * Y + (1 - p) * (1 - Y)
    a_t = alpha_t * Y + (1 - alpha_t) * (1 - Y)
    loss = a_t * ((1 - p_t) ** gamma) * ce * t["weight"]
    return loss.sum() / t["center"].sum().clamp(min=1).float()


def build_loss(cfg):
    name = cfg["loss.name"]
    if name == "centernet":
        a, b = cfg["loss.alpha"], cfg["loss.beta"]
        return lambda logits, t: centernet_focal(logits, t, a, b)
    if name == "bce":
        pw = cfg["loss.pos_weight"]
        return lambda logits, t: bce(logits, t, pw)
    if name == "sigmoid_focal":
        g = cfg["loss.alpha"]
        return lambda logits, t: sigmoid_focal(logits, t, gamma=g)
    raise ValueError(f"unknown loss.name {name!r}")
