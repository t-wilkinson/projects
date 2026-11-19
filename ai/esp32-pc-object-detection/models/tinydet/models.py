"""Backbones, heads and a static profiler (MACs, parameters, peak activation, latency).

Backbone: timm MobileNetV2 at width 0.35 / 0.5 / 0.75 / 1.0, cut at stride 8
(``conv_stem, bn1, blocks[0..2]``). ImageNet weights exist only for 0.5 and 1.0, so
``model.pretrained`` with 0.35 or 0.75 is reported as an invalid config.

Optional parts
- tap:     the next block's 1x1 expansion (6x channels) at stride 8, Edge Impulse's cut point
- context: the whole stride-16 stage, projected and nearest-upsampled back onto the grid.
           Needs RESIZE_NEAREST_NEIGHBOR in the firmware's op resolver.

When pretrained weights are used with a different input normalisation or grayscale, the
stem is re-parameterised exactly (weights scaled, offset folded into BN), so the network
computes the same function and the frozen stem still sees what it was trained on.
"""

from __future__ import annotations

import copy
import math

import torch
import torch.nn as nn
import torch.nn.functional as F

IMAGENET_MEAN = (0.485, 0.456, 0.406)
IMAGENET_STD = (0.229, 0.224, 0.225)
HEAD_DEFAULT_CH = {"basic": 32, "dw1": 32, "dwsep": 64, "bottleneck": 24, "dilated": 24}
WIDTHS = (0.35, 0.5, 0.75, 1.0)


def timm_name(width: float) -> str:
    return f"mobilenetv2_{int(round(width * 100)):03d}"


def has_pretrained(width: float) -> bool:
    import timm
    return bool(timm.list_pretrained(timm_name(width) + "*"))


def check_config(cfg) -> str | None:
    """Why a config can't be built, or None if it can."""
    w = cfg["model.width"]
    if w not in WIDTHS:
        return f"model.width {w} not in {WIDTHS}"
    if cfg["model.pretrained"] and not has_pretrained(w):
        return f"no ImageNet weights for {timm_name(w)}"
    if cfg["model.head"] not in HEAD_DEFAULT_CH:
        return f"unknown model.head {cfg['model.head']!r}"
    if cfg["data.input_size"] % cfg["data.cell_size"]:
        return "data.input_size must be a multiple of data.cell_size"
    return None


def _cbr(c_in, c_out, k=1, groups=1, dilation=1, act=True):
    layers = [nn.Conv2d(c_in, c_out, k, padding=dilation * (k // 2), groups=groups,
                        dilation=dilation, bias=False), nn.BatchNorm2d(c_out)]
    if act:
        layers.append(nn.ReLU6(inplace=True))
    return layers


def build_head(kind: str, c_in: int, ch: int | None, n_out: int, dropout: float) -> nn.Sequential:
    ch = ch or HEAD_DEFAULT_CH[kind]
    if kind == "basic":           # 1x1 only: each cell decides alone
        body = _cbr(c_in, ch)
    elif kind == "dw1":           # one 3x3 depthwise + 1x1 (the July 0b4db56 head)
        body = _cbr(c_in, c_in, 3, groups=c_in, act=False) + _cbr(c_in, ch)
    elif kind == "dwsep":         # two depthwise-separable blocks (current notebook head)
        body = (_cbr(c_in, c_in, 3, groups=c_in) + _cbr(c_in, ch)
                + _cbr(ch, ch, 3, groups=ch) + _cbr(ch, ch))
    elif kind == "bottleneck":    # 1x1 squeeze + two standard 3x3 convs (INT8-friendlier)
        body = _cbr(c_in, ch) + _cbr(ch, ch, 3) + _cbr(ch, ch, 3)
    elif kind == "dilated":       # as bottleneck, second 3x3 dilated: 7x7 receptive field
        body = _cbr(c_in, ch) + _cbr(ch, ch, 3) + _cbr(ch, ch, 3, dilation=2)
    else:
        raise ValueError(kind)
    return nn.Sequential(*body, nn.Dropout2d(p=dropout), nn.Conv2d(ch, n_out, 1, bias=True))


def _reparam_stem(conv: nn.Conv2d, bn: nn.BatchNorm2d, mean, std, gray: bool) -> nn.Conv2d:
    """Fold a new input normalisation (and RGB->gray) into an ImageNet stem, exactly."""
    W = conv.weight.data
    m = torch.tensor(IMAGENET_MEAN).view(1, 3, 1, 1)
    s = torch.tensor(IMAGENET_STD).view(1, 3, 1, 1)
    mean = torch.tensor(mean).view(1, -1, 1, 1)
    std = torch.tensor(std).view(1, -1, 1, 1)
    scale = std / s                     # x_imagenet = scale * x_new + shift
    shift = (mean - m) / s
    W_new = W * scale
    b = (W * shift).sum(dim=(1, 2, 3))
    if gray:
        W_new = W_new.sum(dim=1, keepdim=True)
    new = nn.Conv2d(W_new.shape[1], conv.out_channels, conv.kernel_size, conv.stride,
                    conv.padding, bias=False)
    new.weight.data.copy_(W_new)
    bn.running_mean.data.sub_(b)        # conv_old(x) = conv_new(x) + b
    return new


class TinyDet(nn.Module):
    def __init__(self, cfg, pretrained: bool | None = None):
        super().__init__()
        import timm
        width = cfg["model.width"]
        want_pre = cfg["model.pretrained"] if pretrained is None else pretrained
        use_pre = bool(want_pre and has_pretrained(width))
        gray = cfg["model.grayscale"]
        full = timm.create_model(timm_name(width), pretrained=use_pre,
                                 in_chans=3 if use_pre else (1 if gray else 3))
        if use_pre:
            mean, std = cfg.norm_stats()
            if gray or tuple(mean) != IMAGENET_MEAN or tuple(std) != IMAGENET_STD:
                full.conv_stem = _reparam_stem(full.conv_stem, full.bn1, mean, std, gray)

        self.early = nn.Sequential(full.conv_stem, full.bn1, full.blocks[0], full.blocks[1])
        self.late = full.blocks[2]
        c = self.late[-1].conv_pwl.out_channels
        self.tap = None
        if cfg["model.tap"]:
            ex = full.blocks[3][0]
            self.tap = nn.Sequential(copy.deepcopy(ex.conv_pw), copy.deepcopy(ex.bn1))
            c_head = ex.conv_pw.out_channels
        else:
            c_head = c
        self.ctx = self.ctx_proj = None
        if cfg["model.context"]:
            self.ctx = full.blocks[3]
            c16 = self.ctx[-1].conv_pwl.out_channels
            self.ctx_proj = nn.Sequential(*_cbr(c16, c_head, act=False))
        self.head = build_head(cfg["model.head"], c_head, cfg["model.head_ch"],
                               cfg.num_classes, cfg["model.dropout"])
        with torch.no_grad():  # sigmoid starts near 0.01 (CenterNet init)
            self.head[-1].bias.fill_(math.log(0.01 / 0.99))
        self.pretrained = use_pre
        self.set_frozen(cfg["model.freeze"] == "early" and use_pre)

    def set_frozen(self, frozen: bool):
        self.frozen = frozen
        for p in self.early.parameters():
            p.requires_grad = not frozen

    def train(self, mode: bool = True):
        super().train(mode)
        if mode and getattr(self, "frozen", False):
            self.early.eval()  # frozen layers keep their ImageNet BN statistics
        return self

    def param_groups(self, lr: float, backbone_mult: float, wd: float):
        backbone = list(self.early.parameters()) + list(self.late.parameters())
        if self.tap is not None:
            backbone += list(self.tap.parameters())
        if self.ctx is not None:
            backbone += list(self.ctx.parameters())
        ids = {id(p) for p in backbone}
        head = [p for p in self.parameters() if id(p) not in ids]

        def split(params, lr_):
            decay = [p for p in params if p.ndim > 1]
            no_decay = [p for p in params if p.ndim <= 1]
            return [{"params": decay, "lr": lr_, "weight_decay": wd, "base_lr": lr_},
                    {"params": no_decay, "lr": lr_, "weight_decay": 0.0, "base_lr": lr_}]

        return split(backbone, lr * backbone_mult) + split(head, lr)

    def forward(self, x):
        f = self.late(self.early(x))
        h = self.tap(f) if self.tap is not None else f
        if self.ctx is not None:
            c = self.ctx_proj(self.ctx(f))
            h = h + F.interpolate(c, size=h.shape[-2:], mode="nearest")
        return self.head(h)


class InputPrep:
    """uint8 RGB batch [B, 3, H, W] -> normalised float input for one model.

    Grayscale uses the firmware's fixed-point luma (77R + 150G + 29B) >> 8, so a model
    sees exactly the pixel values the ESP32 would feed it.
    """

    def __init__(self, cfg, device):
        self.gray = cfg["model.grayscale"]
        mean, std = cfg.norm_stats()
        self.mean = torch.tensor(mean, device=device).view(1, -1, 1, 1) * 255.0
        self.std = torch.tensor(std, device=device).view(1, -1, 1, 1) * 255.0
        self.luma = torch.tensor([77, 150, 29], device=device, dtype=torch.int32).view(1, 3, 1, 1)

    def __call__(self, imgs: torch.Tensor) -> torch.Tensor:
        if self.gray:
            x = ((imgs.to(torch.int32) * self.luma).sum(1, keepdim=True) >> 8).float()
        else:
            x = imgs.float()
        return (x - self.mean) / self.std


def build_model(cfg, pretrained: bool | None = None) -> TinyDet:
    err = check_config(cfg) if pretrained is not False else None
    if err:
        raise ValueError(err)
    return TinyDet(cfg, pretrained=pretrained)


# ────────────────────────────────────────────────────────────────────────────
# Static profile
# ────────────────────────────────────────────────────────────────────────────

DEFAULT_LATENCY = {"ms_per_mmac": 7.5, "intercept_ms": 0.0, "calibrated": False}


@torch.no_grad()
def profile(cfg, latency: dict | None = None, model: nn.Module | None = None) -> dict:
    """MACs, parameters (after Conv+BN fusion) and peak int8 activation of one model.

    Peak activation = the largest input+output pair of any conv, a lower bound on the
    TFLite Micro arena. ``pred_ms`` = intercept + ms_per_mmac * MMACs; the default slope
    is a rough figure from Espressif's ESP32-S3 person-detection benchmark (54 ms for a
    ~7.2 MMAC MobileNetV1-0.25) until calibrated with your own invoke() timings.
    """
    lat = {**DEFAULT_LATENCY, **(latency or {})}
    m = model if model is not None else TinyDet(cfg, pretrained=False)
    m.eval()
    stats = {"macs": 0, "params": 0, "peak": 0}

    def hook(mod, inp, out):
        x = inp[0]
        k = mod.kernel_size[0] * mod.kernel_size[1]
        stats["macs"] += out.numel() * (mod.in_channels // mod.groups) * k
        stats["params"] += mod.weight.numel() + mod.out_channels
        stats["peak"] = max(stats["peak"], x.numel() + out.numel())

    hs = [mod.register_forward_hook(hook) for mod in m.modules() if isinstance(mod, nn.Conv2d)]
    s = cfg["data.input_size"]
    m(torch.zeros(1, cfg.in_channels, s, s))
    for h in hs:
        h.remove()
    mmacs = stats["macs"] / 1e6
    return {"mmacs": round(mmacs, 2), "params": int(stats["params"]),
            "peak_kb": round(stats["peak"] / 1024, 1),
            "pred_ms": round(lat["intercept_ms"] + lat["ms_per_mmac"] * mmacs, 1),
            "latency_calibrated": bool(lat.get("calibrated"))}


def fit_latency(points: list[dict]) -> dict:
    """Least-squares ms = a + b * MMACs from measured [{"mmacs": .., "ms": ..}, ...]."""
    if len(points) == 1:
        return {"ms_per_mmac": points[0]["ms"] / points[0]["mmacs"], "intercept_ms": 0.0, "calibrated": True}
    xs = torch.tensor([p["mmacs"] for p in points], dtype=torch.float64)
    ys = torch.tensor([p["ms"] for p in points], dtype=torch.float64)
    A = torch.stack([torch.ones_like(xs), xs], 1)
    sol = torch.linalg.lstsq(A, ys[:, None]).solution.squeeze(1)
    return {"intercept_ms": float(sol[0]), "ms_per_mmac": float(sol[1]), "calibrated": True}
