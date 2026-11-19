"""Evaluation that matches the firmware.

- Peaks: a cell is a detection if it clears the threshold and is the maximum of its 3x3
  neighbourhood, with ties going to the earlier cell in raster order (exactly
  ``firmware/src/detect.rs``), so an INT8 plateau yields one detection, not two.
- Ground truth uses fixed rules (``targets.EVAL_SPEC``), independent of each trial's
  target settings: tiny, crowd and clipped balls are ignore regions.
- Matching: greedy by score, nearest unmatched centre within ``tol`` cells (Euclidean
  on cell indices, the notebook's convention). Detections on ignored cells are dropped.
- Metrics: AP (area under the PR curve), best-threshold F1, F1/P/R at the report
  threshold, FP per image, recall by ball size (every non-crowd ball, tiny ones too),
  and logit percentiles to watch the INT8 output range.
- Confidence quality (the score feeds SLAM as a probability): over all detections,
  ECE (expected calibration error, 10 bins), Brier score and log loss, plus a reliability
  table. Platt scaling p' = sigmoid(a*z + b) is fitted on even-indexed images and scored
  on odd ones (``ece_cal``, ``brier_cal``): it can be folded exactly into the last conv at
  export, so ``ece_cal`` is the calibration the car will actually see. ``platt_a/_b`` are
  fitted on all images for deployment.
"""

from __future__ import annotations

import math
from collections import defaultdict

import numpy as np
import torch
import torch.nn.functional as F

from . import targets as T

SIZE_BUCKETS = [(0, 4), (4, 8), (8, 16), (16, 32), (32, float("inf"))]
MIN_SCORE = 0.05


def bucket_name(lo, hi) -> str:
    return f"{lo}-{hi}" if math.isfinite(hi) else f"{lo}+"


def peak_mask(scores: torch.Tensor, thr: float) -> torch.Tensor:
    """[B, C, G, G] bool: 3x3 local maxima with raster-order tie-breaking, >= thr."""
    G_h, G_w = scores.shape[-2:]
    pad = F.pad(scores, (1, 1, 1, 1), value=float("-inf"))
    keep = scores >= thr
    for dy in (-1, 0, 1):
        for dx in (-1, 0, 1):
            if dy == 0 and dx == 0:
                continue
            n = pad[..., 1 + dy:1 + dy + G_h, 1 + dx:1 + dx + G_w]
            earlier = dy < 0 or (dy == 0 and dx < 0)
            keep &= (scores > n) if earlier else (scores >= n)
    return keep


def decode_peaks(scores: torch.Tensor, thr: float = MIN_SCORE, max_det: int = 10):
    """Per image list of (class_index, gx, gy, score), highest score first."""
    keep = peak_mask(scores, thr)
    out = [[] for _ in range(scores.shape[0])]
    b, c, y, x = torch.nonzero(keep, as_tuple=True)
    s = scores[b, c, y, x]
    for bi, ci, yi, xi, si in zip(b.tolist(), c.tolist(), y.tolist(), x.tolist(), s.tolist()):
        out[bi].append((ci, xi, yi, si))
    return [sorted(d, key=lambda t: -t[3])[:max_det] for d in out]


class Accumulator:
    """Collects matches for one model on one split."""

    def __init__(self, tol: float = 1.0, max_logits: int = 200_000):
        self.tol = tol
        self.scores, self.tps, self.det_img = [], [], []
        self.n_gt = 0
        self.n_img = 0
        self.size_best = []       # (bucket, best score of a nearby same-class detection)
        self.logits = []
        self.n_logits = 0
        self.max_logits = max_logits

    def add(self, logits: torch.Tensor, gt: dict, boxes: torch.Tensor, cell: int):
        scores = torch.sigmoid(logits.float())
        dets = decode_peaks(scores)
        weight = gt["weight"][:, 0].cpu().numpy()
        pos = gt["positive"].cpu().numpy()
        cells = gt["cells"].cpu().numpy()
        bx = boxes.cpu().numpy()
        for b, d in enumerate(dets):
            self.n_img += 1
            gts = [(int(bx[b, m, 0]), cells[b, m, 0], cells[b, m, 1]) for m in np.nonzero(pos[b])[0]]
            self.n_gt += len(gts)
            used = [False] * len(gts)
            for c, x, y, s in d:
                if weight[b, y, x] == 0:
                    continue
                best, best_d = -1, None
                for k, (gc, gx, gy) in enumerate(gts):
                    if used[k] or gc != c:
                        continue
                    dist = math.hypot(x - gx, y - gy)
                    if dist <= self.tol and (best_d is None or dist < best_d):
                        best, best_d = k, dist
                if best >= 0:
                    used[best] = True
                self.scores.append(s)
                self.tps.append(best >= 0)
                self.det_img.append(self.n_img - 1)
            # Recall by size over every non-crowd box, ignored tiny/clipped ones included
            for m in range(bx.shape[1]):
                if bx[b, m, 5] != 1:
                    continue
                size = min(bx[b, m, 3], bx[b, m, 4])
                gx = min(int((bx[b, m, 1] + bx[b, m, 3] / 2) // cell), weight.shape[2] - 1)
                gy = min(int((bx[b, m, 2] + bx[b, m, 4] / 2) // cell), weight.shape[1] - 1)
                near = [s for c, x, y, s in d if c == int(bx[b, m, 0]) and math.hypot(x - gx, y - gy) <= self.tol]
                k = next(i for i, (lo, hi) in enumerate(SIZE_BUCKETS) if lo <= size < hi)
                self.size_best.append((k, max(near) if near else 0.0))
        if self.n_logits < self.max_logits:
            flat = logits.float().flatten()
            take = flat[torch.randperm(flat.numel(), device=flat.device)[: min(4096, flat.numel())]]
            self.logits.append(take.cpu())
            self.n_logits += take.numel()

    def summary(self, report_thr: float = 0.3) -> dict:
        s = np.asarray(self.scores, dtype=np.float64)
        tp = np.asarray(self.tps, dtype=bool)
        out = {"n_img": self.n_img, "n_gt": self.n_gt, "n_det": int(s.size)}
        fp_at = lambda t: float(((s >= t) & ~tp).sum()) / max(1, self.n_img)
        out["fp_per_img"] = fp_at(report_thr)
        if self.n_gt > 0:
            order = np.argsort(-s)
            ctp = np.cumsum(tp[order])
            cfp = np.cumsum(~tp[order])
            recall = ctp / self.n_gt
            precision = ctp / np.maximum(1, ctp + cfp)
            # All-points interpolated AP (precision envelope)
            mrec = np.concatenate([[0.0], recall, [recall[-1] if recall.size else 0.0]])
            mpre = np.concatenate([[1.0], precision, [0.0]])
            mpre = np.maximum.accumulate(mpre[::-1])[::-1]
            idx = np.nonzero(mrec[1:] != mrec[:-1])[0]
            out["ap"] = float(np.sum((mrec[idx + 1] - mrec[idx]) * mpre[idx + 1]))

            def prf(t):
                k = s >= t
                tp_t = float((k & tp).sum())
                fp_t = float((k & ~tp).sum())
                p = tp_t / max(1.0, tp_t + fp_t)
                r = tp_t / self.n_gt
                return p, r, (2 * p * r / (p + r) if p + r > 0 else 0.0)

            grid = np.round(np.arange(0.05, 0.951, 0.01), 2)
            f1s = [prf(t)[2] for t in grid]
            k = int(np.argmax(f1s))
            out["f1_best"], out["thr_best"] = float(f1s[k]), float(grid[k])
            out["precision"], out["recall"], out["f1"] = prf(report_thr)
        sizes = defaultdict(list)
        for k, best in self.size_best:
            sizes[k].append(best >= report_thr)
        for k, (lo, hi) in enumerate(SIZE_BUCKETS):
            if sizes[k]:
                out[f"recall_{bucket_name(lo, hi)}"] = float(np.mean(sizes[k]))
                out[f"count_{bucket_name(lo, hi)}"] = len(sizes[k])
        out.update(calibration(s, tp, np.asarray(self.det_img)))
        if self.logits:
            v = torch.cat(self.logits).numpy()
            for name, q in (("p01", 1), ("p50", 50), ("p99", 99)):
                out[f"logit_{name}"] = float(np.percentile(v, q))
            out["logit_min"], out["logit_max"] = float(v.min()), float(v.max())
        return out


def _reliability(p: np.ndarray, y: np.ndarray, bins: int = 10):
    edges = np.linspace(0, 1, bins + 1)
    k = np.clip(np.digitize(p, edges) - 1, 0, bins - 1)
    conf, acc, n = [], [], []
    for b in range(bins):
        m = k == b
        n.append(int(m.sum()))
        conf.append(float(p[m].mean()) if m.any() else None)
        acc.append(float(y[m].mean()) if m.any() else None)
    ece = sum(nb / max(1, len(p)) * abs(c - a) for nb, c, a in zip(n, conf, acc) if nb)
    return float(ece), {"conf": conf, "acc": acc, "n": n}


def fit_platt(z: np.ndarray, y: np.ndarray, l2: float = 1e-3, iters: int = 100) -> tuple[float, float]:
    """Logistic regression y ~ sigmoid(a*z + b): damped Newton with backtracking."""
    z = np.clip(z.astype(np.float64), -15, 15)
    y = y.astype(np.float64)
    X = np.stack([z, np.ones_like(z)], 1)
    prior = np.array([1.0, 0.0])

    def loss(w):
        t = X @ w
        return float(np.mean(np.logaddexp(0, t) - y * t) + 0.5 * l2 * np.sum((w - prior) ** 2))

    w = prior.copy()
    f = loss(w)
    for _ in range(iters):
        p = 1 / (1 + np.exp(-(X @ w)))
        g = X.T @ (p - y) / len(y) + l2 * (w - prior)
        H = (X * (p * (1 - p))[:, None]).T @ X / len(y) + l2 * np.eye(2)
        step = np.linalg.solve(H, g)
        t = 1.0
        while t > 1e-4:
            w_new = w - t * step
            f_new = loss(w_new)
            if f_new <= f:
                break
            t *= 0.5
        else:
            break
        if abs(f - f_new) < 1e-10:
            w = w_new
            break
        w, f = w_new, f_new
    return float(w[0]), float(w[1])


def calibration(s: np.ndarray, tp: np.ndarray, img: np.ndarray) -> dict:
    """ECE / Brier / log loss of detection scores, raw and after held-out Platt scaling."""
    out = {}
    if s.size == 0:
        return out
    y = tp.astype(np.float64)
    p = np.clip(s, 1e-6, 1 - 1e-6)
    out["ece"], out["reliability"] = _reliability(p, y)
    out["brier"] = float(np.mean((p - y) ** 2))
    out["nll"] = float(-np.mean(y * np.log(p) + (1 - y) * np.log(1 - p)))
    # Too few hits or misses to fit Platt: report the raw values (no correction possible)
    out["ece_cal"], out["brier_cal"], out["reliability_cal"] = out["ece"], out["brier"], out["reliability"]
    out["platt_a"], out["platt_b"] = 1.0, 0.0
    if y.sum() >= 5 and (1 - y).sum() >= 5:
        z = np.log(p / (1 - p))
        out["platt_a"], out["platt_b"] = fit_platt(z, y)
        even = img % 2 == 0
        if y[even].sum() >= 3 and (1 - y[even]).sum() >= 3 and (~even).sum() >= 10:
            a, b = fit_platt(z[even], y[even])
            pc = np.clip(1 / (1 + np.exp(-(a * np.clip(z[~even], -15, 15) + b))), 1e-6, 1 - 1e-6)
            out["ece_cal"], out["reliability_cal"] = _reliability(pc, y[~even])
            out["brier_cal"] = float(np.mean((pc - y[~even]) ** 2))
    return out


@torch.no_grad()
def evaluate(models: dict, loader, cfg, device, report_thr: float = 0.3, tol: float = 1.0,
             max_batches: int | None = None) -> dict:
    """Evaluate several models on one loader pass.

    ``models``: {name: (module, InputPrep)}. Returns {name: metrics dict}.
    """
    acc = {k: Accumulator(tol) for k in models}
    for m, _ in models.values():
        m.eval()
    S, cell, C = cfg["data.input_size"], cfg["data.cell_size"], cfg.num_classes
    amp = device.type == "cuda"
    for i, (imgs, boxes) in enumerate(loader):
        if max_batches is not None and i >= max_batches:
            break
        imgs = imgs.to(device, non_blocking=True)
        boxes = boxes.to(device, non_blocking=True)
        gt = T.render(boxes, C, S, cell, T.EVAL_SPEC)
        for k, (m, prep) in models.items():
            with torch.autocast(device.type, dtype=torch.bfloat16, enabled=amp):
                logits = m(prep(imgs))
            acc[k].add(logits, gt, boxes, cell)
    return {k: a.summary(report_thr) for k, a in acc.items()}


def objective(metrics: dict, spec: dict) -> float | None:
    """Weighted sum of flat metrics, e.g. {"terms": [{"metric": "val_zoom/ap", "weight": 1}]}.

    None if a metric is missing (e.g. a trial evaluated before that metric existed; see
    ``Search.reevaluate``)."""
    total = 0.0
    for term in spec.get("terms", []):
        v = metrics.get(term["metric"])
        if not isinstance(v, (int, float)) or (isinstance(v, float) and math.isnan(v)):
            return None
        total += term["weight"] * v
    return total
