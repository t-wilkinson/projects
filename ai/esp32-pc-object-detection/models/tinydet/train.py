"""Lockstep multi-model training with warmup-stable-decay (WSD) rungs.

Why lockstep: these models are tiny, so a training step is dominated by JPEG decoding
and augmentation on the CPU. Trials that share a data signature (``config.py``) train on
the *same* batches: one loader feeds K models, each with its own targets, loss,
optimiser, normalisation and EMA. K trials cost little more than one.

Why WSD: successive halving needs a fair score at 15 epochs and the option to continue
to 40 and 120 without restarting. A trial trains at a constant LR (after warmup) up to
its rung, saves ``stable.pt``, then runs a short decay branch (``optim.decay_frac`` of
the rung) and is evaluated there. If promoted, it resumes from ``stable.pt``, not from
the decayed weights. ``optim.schedule = "cosine"`` gives a classic single run instead.
"""

from __future__ import annotations

import copy
import math
import random
import time
from dataclasses import dataclass, field
from pathlib import Path

import numpy as np
import torch

from . import data as D
from . import evaluate as E
from . import losses as L
from . import models as M
from . import targets as T
from . import tracking
from .config import Config


# ────────────────────────────────────────────────────────────────────────────
# Pieces
# ────────────────────────────────────────────────────────────────────────────

class EMA:
    """Exponential moving average of weights, with timm-style warmup of the decay."""

    def __init__(self, model: torch.nn.Module, decay: float):
        self.module = copy.deepcopy(model).eval()
        for p in self.module.parameters():
            p.requires_grad_(False)
        self.decay = decay
        self.updates = 0

    @torch.no_grad()
    def update(self, model: torch.nn.Module):
        self.updates += 1
        d = min(self.decay, (1 + self.updates) / (10 + self.updates))
        for e, m in zip(self.module.parameters(), model.parameters()):
            e.mul_(d).add_(m.detach(), alpha=1 - d)
        for e, m in zip(self.module.buffers(), model.buffers()):
            e.copy_(m)

    def state_dict(self):
        return {"module": self.module.state_dict(), "updates": self.updates}

    def load_state_dict(self, sd):
        self.module.load_state_dict(sd["module"])
        self.updates = sd["updates"]


def lr_factor(cfg: Config, epoch_f: float, phase: str, stable_end: int, decay_epochs: int) -> float:
    """LR multiplier at fractional epoch ``epoch_f``."""
    w = cfg["optim.warmup_epochs"]
    lo = cfg["optim.min_lr_frac"]
    if epoch_f < w:
        return max(lo, (epoch_f + 1e-3) / w)
    if cfg["optim.schedule"] == "cosine":
        q = min(1.0, (epoch_f - w) / max(1e-6, stable_end - w))
        return lo + (1 - lo) * 0.5 * (1 + math.cos(math.pi * q))
    if phase == "stable":
        return 1.0
    q = min(1.0, (epoch_f - stable_end) / max(1e-6, decay_epochs))
    return lo + (1 - lo) * 0.5 * (1 + math.cos(math.pi * q))


@dataclass
class JobSpec:
    """One trial's share of a group: train from ``start`` (0 or stable.pt) to rung ``rung``."""
    trial_id: str
    cfg: Config
    trial_dir: Path
    rung: int
    stable_end: int                 # epoch at which the stable phase ends (the rung budget)
    resume: bool = False


@dataclass
class _Job:
    spec: JobSpec
    model: torch.nn.Module
    prep: M.InputPrep
    opt: torch.optim.Optimizer
    ema: EMA | None
    loss_fn: object
    tspec: T.TargetSpec
    epoch: int = 0                  # completed epochs (stable + decay)
    decay_epochs: int = 0
    phase: str = "stable"
    status: str = "running"         # running | done | failed
    error: str | None = None
    losses: list = field(default_factory=list)

    @property
    def total_epochs(self) -> int:
        return self.spec.stable_end + self.decay_epochs

    def eval_module(self):
        return self.ema.module if self.ema is not None else self.model


def _build_job(spec: JobSpec, device) -> _Job:
    cfg = spec.cfg
    torch.manual_seed(cfg["seed"] * 1000 + int(spec.trial_id.split("-")[0].lstrip("t") or 0))
    model = M.build_model(cfg).to(device).to(memory_format=torch.channels_last)
    groups = model.param_groups(cfg["optim.lr"], cfg["model.backbone_lr_mult"], cfg["optim.wd"])
    opt = torch.optim.AdamW(groups, lr=cfg["optim.lr"], betas=(0.9, 0.999))
    ema = EMA(model, cfg["optim.ema"]) if cfg["optim.ema"] > 0 else None
    decay = 0 if cfg["optim.schedule"] == "cosine" else max(1, round(cfg["optim.decay_frac"] * spec.stable_end))
    job = _Job(spec, model, M.InputPrep(cfg, device), opt, ema, L.build_loss(cfg),
               T.TargetSpec.from_config(cfg), decay_epochs=decay)
    if spec.resume:
        ck = torch.load(spec.trial_dir / "stable.pt", map_location=device, weights_only=False)
        model.load_state_dict(ck["model"])
        opt.load_state_dict(ck["opt"])
        if ema is not None and ck.get("ema"):
            ema.load_state_dict(ck["ema"])
        job.epoch = ck["epoch"]
        model.set_frozen(ck.get("frozen", model.frozen))
        if job.epoch >= spec.stable_end and decay > 0:
            job.phase = "decay"  # interrupted during this rung's decay branch: redo the branch
        # drop epoch rows written after this checkpoint by an interrupted run
        mpath = spec.trial_dir / "metrics.jsonl"
        rows = [r for r in tracking.read_jsonl(mpath) if r.get("epoch", 0) <= job.epoch]
        mpath.write_text("".join(__import__("json").dumps(r) + "\n" for r in rows))
    else:
        (spec.trial_dir / "metrics.jsonl").unlink(missing_ok=True)
    return job


def _save_stable(job: _Job):
    torch.save({"model": job.model.state_dict(), "opt": job.opt.state_dict(),
                "ema": job.ema.state_dict() if job.ema else None, "epoch": job.epoch,
                "frozen": job.model.frozen, "config": job.spec.cfg.to_dict()},
               job.spec.trial_dir / "stable.pt")


# ────────────────────────────────────────────────────────────────────────────
# Data context (index, splits, cached eval loaders)
# ────────────────────────────────────────────────────────────────────────────

class DataContext:
    def __init__(self, index: dict, splits: dict, coco_root: str, workers: int = 8,
                 eval_batch: int = 256, cache_dir: str | Path = "cache"):
        self.index, self.splits = index, splits
        self.coco_root = coco_root
        self.workers = workers
        self.eval_batch = eval_batch
        self.cache_dir = Path(cache_dir)
        self._eval: dict = {}
        self._banks: dict = {}

    def cutout_bank(self, min_px: float = 20):
        """Copy-paste cutouts from training images (built once, cached on disk)."""
        if min_px not in self._banks:
            from .paste import CutoutBank
            self._banks[min_px] = CutoutBank.build(self.index, self.splits, self.coco_root,
                                                   self.cache_dir, min_px=min_px)
        return self._banks[min_px]

    def eval_loader(self, name: str, cfg: Config):
        key = (name, cfg["data.input_size"], tuple(cfg.class_ids))
        if key not in self._eval:
            ds = D.eval_dataset(name, cfg.override({"data.coco_root": self.coco_root}), self.index, self.splits)
            # not persistent: a dozen idle eval worker pools would sit in RAM between rungs
            self._eval[key] = D.make_loader(ds, self.eval_batch, shuffle=False, workers=self.workers,
                                            persistent=False)
        return self._eval[key]

    def train_loader(self, cfg: Config, seed: int):
        bank = self.cutout_bank(cfg["data.paste_bank_min_px"]) if cfg["data.paste_prob"] > 0 else None
        ds = D.train_dataset(cfg.override({"data.coco_root": self.coco_root}), self.index, self.splits, bank=bank)
        return D.make_loader(ds, cfg["optim.batch_size"], shuffle=True, workers=self.workers,
                             seed=seed, drop_last=True)


# ────────────────────────────────────────────────────────────────────────────
# Group trainer
# ────────────────────────────────────────────────────────────────────────────

def run_group(specs: list[JobSpec], ctx: DataContext, ledger: tracking.Ledger, objective_spec: dict,
              device=None, eval_every: int = 5,
              report_thr: float = 0.3, mlflow: tracking.MlflowMirror | None = None,
              log=print) -> dict[str, dict]:
    """Train every job to its rung in lockstep; evaluate each at the end of its decay branch.

    All specs must share ``cfg.data_signature()``. Returns {trial_id: rung metrics}.
    """
    device = device or torch.device("cuda" if torch.cuda.is_available() else "cpu")
    sigs = {s.cfg.data_signature() for s in specs}
    assert len(sigs) == 1, f"jobs in one group must share the data signature, got {sigs}"
    data_cfg = specs[0].cfg
    group_seed = random.randrange(2**31)
    loader = ctx.train_loader(data_cfg, seed=group_seed)
    steps = len(loader)
    S, cell, C = data_cfg["data.input_size"], data_cfg["data.cell_size"], data_cfg.num_classes
    amp = device.type == "cuda" and data_cfg["optim.amp"]

    jobs = []
    for sp in specs:
        sp.trial_dir.mkdir(parents=True, exist_ok=True)
        try:
            jobs.append(_build_job(sp, device))
        except Exception as ex:  # bad config: record and continue with the others
            ledger.append("trial_failed", trial=sp.trial_id, rung=sp.rung, error=repr(ex))
            log(f"  {sp.trial_id}: failed to build ({ex})")
    results: dict[str, dict] = {}
    log(f"group {next(iter(sigs))}: {len(jobs)} model(s), {steps} steps/epoch, "
        + ", ".join(f"{j.spec.trial_id} e{j.epoch}->{j.total_epochs}" for j in jobs))

    while any(j.status == "running" for j in jobs):
        t0 = time.time()
        active = [j for j in jobs if j.status == "running"]
        for j in active:
            j.model.train()
            j.losses = []
            ue = j.spec.cfg["model.unfreeze_epoch"]
            if ue >= 0 and j.epoch >= ue and j.model.frozen:
                j.model.set_frozen(False)
        for it, (imgs, boxes) in enumerate(loader):
            imgs = imgs.to(device, non_blocking=True).contiguous(memory_format=torch.channels_last)
            boxes = boxes.to(device, non_blocking=True)
            for j in active:
                if j.status != "running":
                    continue
                f = lr_factor(j.spec.cfg, j.epoch + it / steps, j.phase, j.spec.stable_end, j.decay_epochs)
                for g in j.opt.param_groups:
                    g["lr"] = g["base_lr"] * f
                t = T.render(boxes, C, S, cell, j.tspec)
                with torch.autocast(device.type, dtype=torch.bfloat16, enabled=amp):
                    logits = j.model(j.prep(imgs))
                loss = j.loss_fn(logits, t)
                if not torch.isfinite(loss):
                    j.status, j.error = "failed", f"non-finite loss at epoch {j.epoch} step {it}"
                    continue
                j.opt.zero_grad(set_to_none=True)
                loss.backward()
                if j.spec.cfg["optim.grad_clip"]:
                    torch.nn.utils.clip_grad_norm_(j.model.parameters(), j.spec.cfg["optim.grad_clip"])
                j.opt.step()
                if j.ema is not None:
                    j.ema.update(j.model)
                j.losses.append(loss.item())

        dt_epoch = time.time() - t0
        rows: dict[str, dict] = {}
        for j in active:
            if j.status == "failed":
                ledger.append("trial_failed", trial=j.spec.trial_id, rung=j.spec.rung, error=j.error)
                log(f"  {j.spec.trial_id}: {j.error}")
                continue
            j.epoch += 1
            row = {"epoch": j.epoch, "phase": j.phase, "loss": float(np.mean(j.losses)) if j.losses else None,
                   "lr": j.opt.param_groups[-1]["lr"], "sec": round(dt_epoch, 1)}
            if j.phase == "stable" and j.epoch == j.spec.stable_end:
                if j.decay_epochs > 0:
                    _save_stable(j)
                    j.phase = "decay"
            rows[j.spec.trial_id] = row
        # quick learning-curve check: every model due this epoch, one pass over a small subset
        due = [j for j in active if j.status == "running" and eval_every and j.epoch % eval_every == 0
               and j.epoch < j.total_epochs]
        if due:
            q = E.evaluate({j.spec.trial_id: (j.eval_module(), j.prep) for j in due},
                           ctx.eval_loader("val_zoom_quick", data_cfg), data_cfg, device, report_thr)
            for j in due:
                rows[j.spec.trial_id].update({f"quick/{k}": v for k, v in q[j.spec.trial_id].items()
                                              if k in ("ap", "f1_best", "f1", "ece")})
            # (models go back to train mode at the start of the next epoch)
        for j in active:
            if j.spec.trial_id in rows:
                tracking.append_jsonl(j.spec.trial_dir / "metrics.jsonl", rows.pop(j.spec.trial_id))
        finished = [j for j in active if j.status == "running" and j.epoch >= j.total_epochs]
        for j in finished:
            j.status = "done"
        done = sum(j.status != "running" for j in jobs)
        msg = " ".join(f"{j.spec.trial_id}:{np.mean(j.losses):.3f}" for j in active if j.losses)
        log(f"  epoch {max(j.epoch for j in jobs)} {dt_epoch:.0f}s {done}/{len(jobs)} done | loss {msg}")
        for j in finished:
            if j.decay_epochs == 0:
                _save_stable(j)
            results[j.spec.trial_id] = _finish(j, ctx, ledger, objective_spec, device, report_thr, mlflow, log)
    return results


def _finish(job: _Job, ctx: DataContext, ledger, objective_spec, device, report_thr, mlflow, log) -> dict:
    cfg = job.spec.cfg
    module = job.eval_module()
    metrics = {}
    for name in D.EVAL_SPLITS:
        m = E.evaluate({"m": (module, job.prep)}, ctx.eval_loader(name, cfg), cfg, device, report_thr)["m"]
        metrics.update({f"{name}/{k}": v for k, v in m.items()})
    obj = E.objective(metrics, objective_spec)
    r = job.spec.rung
    torch.save({"model": module.state_dict(), "config": cfg.to_dict(), "epoch": job.epoch,
                "metrics": metrics}, job.spec.trial_dir / f"rung{r}.pt")
    (job.spec.trial_dir / f"eval_r{r}.json").write_text(
        __import__("json").dumps({"epoch": job.epoch, "objective": obj, "metrics": metrics}, indent=2))
    ledger.append("rung_done", trial=job.spec.trial_id, rung=r, epochs=job.spec.stable_end,
                  trained_epochs=job.epoch, objective=obj, metrics=metrics)
    if mlflow is not None:
        mlflow.log_rung(job.spec.trial_id, cfg.diff(), r, job.epoch, {**metrics, "objective": obj or 0.0})
    show = {k: metrics.get(k) for k in ("val_zoom/ap", "val_dev/ap", "val_full/f1", "val_neg/fp_per_img",
                                         "val_zoom/ece_cal")}
    log(f"  {job.spec.trial_id} rung {r} ({job.epoch} ep): objective={obj if obj is None else round(obj, 4)} "
        + " ".join(f"{k}={v:.3f}" for k, v in show.items() if isinstance(v, float)))
    return {"objective": obj, **metrics}
