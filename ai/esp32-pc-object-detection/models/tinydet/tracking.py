"""Experiment folders and the append-only JSONL ledger.

experiments/<name>/
    ledger.jsonl            one JSON object per event, the source of truth
    trials/<trial_id>/
        config.json         full config of the trial
        metrics.jsonl       one row per epoch (loss, lr, quick val metrics)
        stable.pt           checkpoint at the end of the last stable phase (to resume)
        rung<k>.pt          weights after the rung-k decay branch (EMA if enabled)
        eval_r<k>.json      full evaluation at rung k
    observed.json           what the search has learned per parameter (rebuilt)
    dashboard.html          static dashboard (rebuilt)

Ledger events: experiment_started, trial_created, trial_infeasible, rung_done,
promoted, trial_failed, stage_closed, note. Each carries ``time`` and usually ``trial``.
An optional MLflow mirror logs the same params and metrics, so ``mlflow ui`` works too.
"""

from __future__ import annotations

import datetime as dt
import json
import platform
import subprocess
from pathlib import Path
from typing import Any


def now() -> str:
    return dt.datetime.now().isoformat(timespec="seconds")


class Ledger:
    def __init__(self, path: str | Path):
        self.path = Path(path)
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self.path.touch(exist_ok=True)

    def append(self, event: str, **fields: Any) -> dict:
        row = {"event": event, "time": now(), **fields}
        with self.path.open("a") as f:
            f.write(json.dumps(row, default=_json_default) + "\n")
        return row

    def read(self) -> list[dict]:
        rows = []
        for line in self.path.read_text().splitlines():
            line = line.strip()
            if line:
                try:
                    rows.append(json.loads(line))
                except json.JSONDecodeError:
                    pass  # a line cut by a crash; skip it
        return rows


def append_jsonl(path: str | Path, row: dict) -> None:
    with Path(path).open("a") as f:
        f.write(json.dumps(row, default=_json_default) + "\n")


def read_jsonl(path: str | Path) -> list[dict]:
    p = Path(path)
    if not p.exists():
        return []
    out = []
    for line in p.read_text().splitlines():
        try:
            out.append(json.loads(line))
        except json.JSONDecodeError:
            pass
    return out


def _json_default(o):
    if hasattr(o, "item"):
        return o.item()
    if isinstance(o, Path):
        return str(o)
    return str(o)


def git_info(path: str | Path = ".") -> dict:
    def run(*args):
        try:
            return subprocess.check_output(["git", *args], cwd=path, stderr=subprocess.DEVNULL,
                                           text=True).strip()
        except Exception:
            return None
    commit = run("rev-parse", "--short", "HEAD")
    dirty = run("status", "--porcelain")
    return {"commit": commit, "dirty": bool(dirty) if dirty is not None else None}


def env_info() -> dict:
    import torch
    info = {"python": platform.python_version(), "torch": torch.__version__,
            "cuda": torch.cuda.is_available()}
    if torch.cuda.is_available():
        info["gpu"] = torch.cuda.get_device_name(0)
    return info


class MlflowMirror:
    """Mirror trials into a local MLflow store (``mlflow ui --backend-store-uri <dir>``).

    Disabled silently when mlflow isn't installed.
    """

    def __init__(self, exp_dir: str | Path, enabled: bool = True):
        self.ok = False
        if not enabled:
            return
        try:
            import mlflow
        except ImportError:
            return
        self.mlflow = mlflow
        uri = (Path(exp_dir).resolve() / "mlruns").as_uri()
        mlflow.set_tracking_uri(uri)
        mlflow.set_experiment(Path(exp_dir).name)
        self.ok = True

    def log_rung(self, trial_id: str, params: dict, rung: int, epochs: int, metrics: dict):
        if not self.ok:
            return
        mf = self.mlflow
        clean = {k.replace("/", "."): v for k, v in metrics.items() if isinstance(v, (int, float))}
        with mf.start_run(run_name=f"{trial_id}-r{rung}"):
            mf.set_tags({"trial": trial_id, "rung": rung})
            mf.log_params({k: str(v)[:250] for k, v in params.items()})
            mf.log_metrics(clean, step=epochs)
