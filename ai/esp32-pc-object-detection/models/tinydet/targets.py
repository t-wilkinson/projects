"""Render training targets on the GPU from raw boxes.

Boxes arrive as a padded tensor [B, M, 6] with rows (class_index, x, y, w, h, flag) in
input pixels; flag 0 = padding, 1 = normal, 2 = crowd. Rendering per model on the GPU
means trials with different target settings can share one data stream.

Box roles
- positive: normal box, larger than ``ignore_px``, not clipped by the image border
- ignored : crowd boxes, tiny boxes, and clipped balls (touch the border and are clearly
            not round). Their cells get loss weight 0: neither reward nor punish.

Heatmap forms (``target.form``)
- gauss_peak       Gaussian centred on the continuous box centre, 1.0 forced at its cell
                   (the original notebook's form, with the half-cell offset fixed)
- gauss_cell       Gaussian centred on the centre cell (standard CenterNet); peak = 1.0
- onehot           1.0 at the centre cell only (original FOMO)
- onehot_nbr_ignore  one-hot, with the 8 neighbours ignored so a 1-cell miss is free
"""

from __future__ import annotations

from dataclasses import dataclass

import torch


@dataclass
class TargetSpec:
    form: str = "gauss_cell"
    sigma: float = 0.7
    sigma_scale: float = 0.0
    ignore_px: float = 3.5
    ignore_footprint: int = 3
    clipped_aspect: float = 0.6
    legacy_offset: bool = False

    @classmethod
    def from_config(cls, cfg) -> "TargetSpec":
        s = cfg.section("target")
        return cls(**{k: s[k] for k in cls.__dataclass_fields__})


# The evaluation always uses the same rules, so trials with different target
# settings are scored on the same ground truth.
EVAL_SPEC = TargetSpec(form="onehot", ignore_px=3.5, ignore_footprint=3, clipped_aspect=0.6)


def box_roles(boxes: torch.Tensor, input_size: int, spec: TargetSpec):
    """Boolean masks (positive, ignored) of shape [B, M]."""
    x, y, w, h, flag = boxes[..., 1], boxes[..., 2], boxes[..., 3], boxes[..., 4], boxes[..., 5]
    valid = flag > 0
    crowd = flag == 2
    tiny = (w * h) <= spec.ignore_px ** 2
    edge = 0.5
    touches = (x <= edge) | (y <= edge) | (x + w >= input_size - edge) | (y + h >= input_size - edge)
    aspect = torch.minimum(w, h) / torch.clamp(torch.maximum(w, h), min=1e-6)
    clipped = touches & (aspect < spec.clipped_aspect)
    positive = valid & ~crowd & ~tiny & ~clipped
    ignored = valid & ~positive
    return positive, ignored


def render(boxes: torch.Tensor, num_classes: int, input_size: int, cell_size: int,
           spec: TargetSpec) -> dict[str, torch.Tensor]:
    """Targets for one batch.

    Returns
        heatmap [B, C, G, G] float in [0, 1]
        center  [B, C, G, G] bool, True at positive centre cells
        weight  [B, 1, G, G] float, 0 where the loss is ignored
        positive, ignored [B, M] bool box roles; cells [B, M, 2] long (gx, gy)
    """
    device = boxes.device
    B, M, _ = boxes.shape
    G = input_size // cell_size
    cls = boxes[..., 0].long().clamp(0, num_classes - 1)
    x, y, w, h = boxes[..., 1], boxes[..., 2], boxes[..., 3], boxes[..., 4]
    cx, cy = (x + w / 2) / cell_size, (y + h / 2) / cell_size          # grid units, continuous
    gx = cx.floor().long().clamp(0, G - 1)
    gy = cy.floor().long().clamp(0, G - 1)
    positive, ignored = box_roles(boxes, input_size, spec)

    idx = torch.arange(G, device=device, dtype=torch.float32)
    centres = idx if spec.legacy_offset else idx + 0.5                 # cell g is centred at g + 0.5
    ys = centres.view(1, 1, G, 1)
    xs = centres.view(1, 1, 1, G)

    # Centre cells (one-hot), per class
    onehot_cls = torch.nn.functional.one_hot(cls, num_classes).bool() & positive[..., None]  # [B,M,C]
    flat = (gy * G + gx)                                               # [B,M]
    center = torch.zeros(B, num_classes, G * G, dtype=torch.bool, device=device)
    if positive.any():
        b_i, m_i = torch.nonzero(positive, as_tuple=True)
        center[b_i, cls[b_i, m_i], flat[b_i, m_i]] = True
    center = center.view(B, num_classes, G, G)

    if spec.form in ("gauss_peak", "gauss_cell"):
        if spec.form == "gauss_peak":
            mx, my = cx, cy
        else:
            mx, my = gx.float() + 0.5, gy.float() + 0.5
        diam = torch.minimum(w, h) / cell_size
        sigma = torch.clamp(diam * spec.sigma_scale, min=spec.sigma) if spec.sigma_scale > 0 \
            else torch.full_like(diam, spec.sigma)
        d2 = (xs - mx[..., None, None]) ** 2 + (ys - my[..., None, None]) ** 2          # [B,M,G,G]
        g = torch.exp(-d2 / (2 * sigma[..., None, None] ** 2)) * positive[..., None, None]
        heat = (g[:, :, None] * onehot_cls[..., None, None]).amax(dim=1)                # [B,C,G,G]
        heat = torch.maximum(heat, center.float())
    elif spec.form in ("onehot", "onehot_nbr_ignore"):
        heat = center.float()
    else:
        raise ValueError(f"unknown target.form {spec.form!r}")

    # Ignore regions
    r = spec.ignore_footprint // 2
    gyy = torch.arange(G, device=device).view(1, 1, G, 1)
    gxx = torch.arange(G, device=device).view(1, 1, 1, G)
    near = ((gyy - gy[..., None, None]).abs() <= r) & ((gxx - gx[..., None, None]).abs() <= r)  # [B,M,G,G]
    # Crowd boxes also ignore every cell they overlap
    x0c, y0c = (x / cell_size)[..., None, None], (y / cell_size)[..., None, None]
    x1c, y1c = ((x + w) / cell_size)[..., None, None], ((y + h) / cell_size)[..., None, None]
    overlap = (gxx + 1 > x0c) & (gxx < x1c) & (gyy + 1 > y0c) & (gyy < y1c)
    crowd = (boxes[..., 5] == 2)[..., None, None]
    ign_region = ((near | (overlap & crowd)) & ignored[..., None, None]).any(dim=1)      # [B,G,G]
    if spec.form == "onehot_nbr_ignore":
        nbr = ((gyy - gy[..., None, None]).abs() <= 1) & ((gxx - gx[..., None, None]).abs() <= 1)
        ign_region |= (nbr & positive[..., None, None]).any(dim=1)
    weight = (~ign_region).float()[:, None]
    weight[center.any(dim=1, keepdim=True)] = 1.0                       # a real centre is always scored

    cells = torch.stack([gx, gy], dim=-1)
    return {"heatmap": heat, "center": center, "weight": weight,
            "positive": positive, "ignored": ignored, "cells": cells}
