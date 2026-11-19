"""Prior-guided, staged hyperparameter search driven by ``search/space.json``.

The two files are the search's memory:
- ``space.json`` (you edit it): every parameter with its range, a prior over values,
  your belief in its ``impact`` and your ``confidence`` in the prior, plus stages,
  budgets, constraints and the objective. Re-read on every planning step, so edits
  take effect between groups without restarting anything.
- ``ledger.jsonl`` (the search appends to it): every trial, rung result, promotion and
  stage decision. Re-running the notebook resumes from it.

Each planning step
1. Picks the first open stage. Parameters of that stage are searched; parameters of
   closed stages are pinned to that stage's winner; ``fixed`` in space.json overrides
   both; everything else comes from ``base`` and the config defaults.
2. Promotes trials by asynchronous successive halving (ASHA): a trial that finished rung
   r continues to rung r+1 if it is in the top 1/eta of trials that finished rung r.
3. Fills the rest of the group with new trials: the stage's ``seeds`` first, then
   ``startup`` draws from your priors, then Optuna's TPE fitted on rung-0 results.
   New trials in one group share their data parameters, so they train in lockstep.
4. Skips configs that can't be built or break the constraints (latency, arena, params)
   before spending GPU time on them.
5. Closes the stage when its trial budget is spent and the best trial has reached the
   last rung; the winner's values are pinned for later stages.
"""

from __future__ import annotations

import json
import math
import random
from collections import defaultdict
from pathlib import Path

from . import evaluate as E
from . import models as M
from . import tracking
from .config import DEFAULTS, Config
from .train import DataContext, JobSpec, run_group

DATA_KEYS = tuple(k for k in DEFAULTS if k.startswith("data.")) + ("optim.batch_size",)


# ────────────────────────────────────────────────────────────────────────────
# Space
# ────────────────────────────────────────────────────────────────────────────

def load_space(path: str | Path) -> dict:
    space = json.loads(Path(path).read_text())
    for k in list(space.get("params", {})) + list(space.get("base", {})) + list(space.get("fixed", {})):
        if k not in DEFAULTS:
            raise KeyError(f"space.json: unknown config key {k!r}")
    names = [s["name"] for s in space["stages"]]
    for k, p in space["params"].items():
        if p.get("stage") not in names + [None, "off"]:
            raise ValueError(f"space.json: {k} has stage {p.get('stage')!r}, not one of {names}")
    return space


def sample_prior(p: dict, rng: random.Random):
    """One draw from a parameter's prior; ``confidence`` = chance of taking the mode."""
    conf = float(p.get("confidence", 0.0))
    if p["type"] == "categorical":
        choices = p["choices"]
        w = p.get("prior") or [1.0] * len(choices)
        if rng.random() < conf:
            return choices[max(range(len(w)), key=lambda i: w[i])]
        return rng.choices(choices, weights=w)[0]
    lo, hi, log = p["low"], p["high"], p.get("log", False)
    mode = (p.get("prior") or {}).get("mode")
    tf = (math.log if log else (lambda v: v))
    inv = (math.exp if log else (lambda v: v))
    if mode is not None and rng.random() < max(conf, 0.5):
        spread = (p.get("prior") or {}).get("spread", 0.25) * (tf(hi) - tf(lo))
        v = inv(min(tf(hi), max(tf(lo), rng.gauss(tf(mode), spread))))
    else:
        v = inv(rng.uniform(tf(lo), tf(hi)))
    if p["type"] == "int":
        step = p.get("step", 1)
        v = int(round((v - lo) / step) * step + lo)
    return v


def _distribution(p: dict):
    import optuna.distributions as od
    if p["type"] == "categorical":
        return od.CategoricalDistribution(p["choices"])
    if p["type"] == "int":
        return od.IntDistribution(p["low"], p["high"], log=p.get("log", False), step=p.get("step", 1))
    return od.FloatDistribution(p["low"], p["high"], log=p.get("log", False))


# ────────────────────────────────────────────────────────────────────────────
# Ledger state
# ────────────────────────────────────────────────────────────────────────────

class State:
    """Trials, rung results and stage decisions parsed from the ledger.

    With ``objective`` given, every rung's objective is recomputed from its stored metrics,
    so editing the objective in space.json re-scores the whole history.
    """

    def __init__(self, rows: list[dict], objective: dict | None = None):
        self.trials: dict[str, dict] = {}
        self.closed: dict[str, dict] = {}
        self.infeasible: list[dict] = []
        for r in rows:
            ev = r["event"]
            if ev == "trial_created":
                self.trials[r["trial"]] = {"id": r["trial"], "stage": r["stage"], "params": r["params"],
                                           "config": r["config"], "origin": r.get("origin"),
                                           "profile": r.get("profile", {}), "rungs": {},
                                           "promoted": set(), "failed": False, "error": None}
            elif ev == "rung_done" and r["trial"] in self.trials:
                if objective is not None and r.get("metrics"):
                    r = {**r, "objective": E.objective(r["metrics"], objective)}
                self.trials[r["trial"]]["rungs"][int(r["rung"])] = r
            elif ev == "promoted" and r["trial"] in self.trials:
                self.trials[r["trial"]]["promoted"].add(int(r["to_rung"]))
            elif ev == "trial_failed" and r["trial"] in self.trials:
                self.trials[r["trial"]]["failed"] = True
                self.trials[r["trial"]]["error"] = r.get("error")
            elif ev == "stage_closed":
                self.closed[r["stage"]] = r
            elif ev == "trial_infeasible":
                self.infeasible.append(r)

    def stage_trials(self, stage: str) -> list[dict]:
        return [t for t in self.trials.values() if t["stage"] == stage]

    def next_id(self) -> int:
        nums = [int(t.split("-")[0][1:]) for t in self.trials]
        return max(nums, default=0) + 1


def top_rung(t: dict) -> int:
    return max(t["rungs"], default=-1)


def score(t: dict, rung: int | None = None) -> float | None:
    r = top_rung(t) if rung is None else rung
    return t["rungs"].get(r, {}).get("objective") if r >= 0 else None


# ────────────────────────────────────────────────────────────────────────────
# Search
# ────────────────────────────────────────────────────────────────────────────

class Search:
    def __init__(self, exp_dir: str | Path, space_path: str | Path, ctx: DataContext,
                 device=None, mlflow: bool = False, seed: int | None = None, log=print):
        self.exp = Path(exp_dir)
        self.space_path = Path(space_path)
        self.ctx = ctx
        self.device = device
        self.ledger = tracking.Ledger(self.exp / "ledger.jsonl")
        self.mlflow = tracking.MlflowMirror(self.exp, enabled=mlflow) if mlflow else None
        self.rng = random.Random(seed)
        self.log = log
        if not self.ledger.read():
            self.ledger.append("experiment_started", space=str(self.space_path),
                               git=tracking.git_info(self.space_path.parent), env=tracking.env_info())

    # ── views ──────────────────────────────────────────────────────────────
    @property
    def space(self) -> dict:
        return load_space(self.space_path)

    def state(self) -> State:
        return State(self.ledger.read(), self.space["objective"])

    def current_stage(self, space=None, st=None) -> dict | None:
        space, st = space or self.space, st or self.state()
        for s in space["stages"]:
            if s["name"] not in st.closed:
                return s
        return None

    def pinned(self, space=None, st=None) -> dict:
        """Values fixed by closed stages, then by space['fixed']."""
        space, st = space or self.space, st or self.state()
        out = {}
        for s in space["stages"]:
            if s["name"] in st.closed:
                out.update(st.closed[s["name"]].get("fixed", {}))
        out.update(space.get("fixed", {}))
        return out

    def base_config(self, space=None, st=None) -> Config:
        space = space or self.space
        return Config({**space.get("base", {}), **self.pinned(space, st)})

    # ── planning ───────────────────────────────────────────────────────────
    def _rungs(self, space, stage) -> list[int]:
        return stage.get("rungs") or space["schedule"]["rungs"]

    def _promotions(self, space, stage, st) -> list[tuple[dict, int]]:
        eta = stage.get("eta", space["schedule"].get("eta", 3))
        rungs = self._rungs(space, stage)
        trials = [t for t in st.stage_trials(stage["name"]) if not t["failed"]]
        out = []
        for r in range(len(rungs) - 1):
            done_r = [t for t in trials if r in t["rungs"] and t["rungs"][r].get("objective") is not None]
            done_r.sort(key=lambda t: -t["rungs"][r]["objective"])
            k = len(done_r) // eta
            for t in done_r[:k]:
                if (r + 1) not in t["promoted"] and top_rung(t) == r:
                    out.append((t, r + 1))
        return out

    def _orphans(self, space, st) -> list[tuple[dict, int, bool]]:
        out = []
        names = {s["name"] for s in space["stages"]}
        for t in st.trials.values():
            if t["failed"] or t["stage"] in st.closed or t["stage"] not in names:
                continue  # manual runs are never resumed automatically
            if 0 not in t["rungs"]:
                out.append((t, 0, False))
            for r in sorted(t["promoted"]):
                if r not in t["rungs"]:
                    out.append((t, r, True))
        return out

    def _finalize(self, space, stage, st) -> tuple[dict, int] | None:
        """When the budget is spent: make sure the best trial reaches the last rung."""
        rungs = self._rungs(space, stage)
        trials = [t for t in st.stage_trials(stage["name"]) if not t["failed"] and t["rungs"]]
        if not trials:
            return None
        hi = max(top_rung(t) for t in trials)
        best = max((t for t in trials if top_rung(t) == hi), key=lambda t: score(t) or -1e9)
        if hi < len(rungs) - 1 and (hi + 1) not in best["promoted"]:
            return best, hi + 1
        return None

    def _close_stage(self, space, stage, st):
        trials = [t for t in st.stage_trials(stage["name"]) if not t["failed"] and t["rungs"]]
        stage_params = [k for k, p in space["params"].items() if p.get("stage") == stage["name"]]
        if trials:
            hi = max(top_rung(t) for t in trials)
            best = max((t for t in trials if top_rung(t) == hi), key=lambda t: score(t) or -1e9)
            fixed = {k: best["config"][k] for k in stage_params}
            self.ledger.append("stage_closed", stage=stage["name"], winner=best["id"],
                               objective=score(best), rung=hi, fixed=fixed)
            self.log(f"stage {stage['name']} closed: winner {best['id']} objective={score(best):.4f} "
                     f"pinned {fixed}")
        else:
            self.ledger.append("stage_closed", stage=stage["name"], winner=None, fixed={})
            self.log(f"stage {stage['name']} closed with no completed trials")

    def _check(self, space, cfg: Config) -> tuple[str | None, dict]:
        err = M.check_config(cfg)
        if err:
            return err, {}
        prof = M.profile(cfg, space.get("latency_model"))
        c = space.get("constraints", {})
        if "max_pred_ms" in c and prof["pred_ms"] > c["max_pred_ms"]:
            return f"pred_ms {prof['pred_ms']} > {c['max_pred_ms']}", prof
        if "max_peak_kb" in c and prof["peak_kb"] > c["max_peak_kb"]:
            return f"peak_kb {prof['peak_kb']} > {c['max_peak_kb']}", prof
        if "max_params" in c and prof["params"] > c["max_params"]:
            return f"params {prof['params']} > {c['max_params']}", prof
        return None, prof

    def _new_trials(self, space, stage, st, n: int, force: dict | None) -> list[tuple[dict, str]]:
        """Up to n new (params, origin) for this stage; ``force`` pins data params."""
        params = {k: p for k, p in space["params"].items() if p.get("stage") == stage["name"]}
        existing = st.stage_trials(stage["name"])
        seen = {Config({**self.base_config(space, st).to_dict(), **t["params"]}).hash() for t in existing}
        out: list[tuple[dict, str]] = []
        seeds = [s for s in stage.get("seeds", []) if not any(t["params"] == s for t in existing)]
        startup = stage.get("startup", max(4, len(params) * 2))
        n_done = len([t for t in existing if 0 in t["rungs"]])
        study = None
        tries = 0
        while len(out) < n and tries < 50 * n:
            tries += 1
            if seeds:
                cand, origin = dict(seeds.pop(0)), "seed"
            elif not params:
                break
            elif n_done < startup or study is False:
                cand, origin = {k: sample_prior(p, self.rng) for k, p in params.items()}, "prior"
            else:
                if study is None:
                    study = self._tpe_study(params, existing) or False
                    if study is False:
                        continue
                fixed = {k: v for k, v in (force or {}).items() if k in params}
                cand, origin = self._tpe_ask(study, params, fixed), "tpe"
            if force:
                cand.update({k: v for k, v in force.items() if k in params})
            cfg = self.base_config(space, st).override(cand)
            if force and any(cfg[kk] != force[kk] for kk in force if kk in DATA_KEYS):
                continue  # e.g. a seed with other data settings: it runs in a later group
            h = cfg.hash()
            if h in seen:
                continue
            err, prof = self._check(space, cfg)
            if err:
                self.ledger.append("trial_infeasible", stage=stage["name"], params=cand, reason=err)
                continue
            seen.add(h)
            out.append((cand, origin))
            if force is None:  # later trials in this group share the first one's data params
                force = {k: cfg[k] for k in DATA_KEYS}
        return out

    def _tpe_study(self, params: dict, existing: list[dict]):
        try:
            import optuna
        except ImportError:
            self.log("optuna not installed: sampling from priors only")
            return None
        optuna.logging.set_verbosity(optuna.logging.WARNING)
        import warnings
        warnings.filterwarnings("ignore", category=optuna.exceptions.ExperimentalWarning)
        dists = {k: _distribution(p) for k, p in params.items()}
        sampler = optuna.samplers.TPESampler(seed=self.rng.randrange(2**31), multivariate=True,
                                             constant_liar=True, n_startup_trials=0)
        study = optuna.create_study(direction="maximize", sampler=sampler)
        for t in existing:
            v = t["rungs"].get(0, {}).get("objective")
            p = {k: t["config"].get(k) for k in params}
            try:
                if v is None:
                    study.add_trial(optuna.trial.create_trial(params=p, distributions=dists,
                                                              state=optuna.trial.TrialState.FAIL))
                else:
                    study.add_trial(optuna.trial.create_trial(params=p, distributions=dists, value=v))
            except ValueError:
                pass  # value outside the current range after an edit of space.json
        study._tinydet_dists = dists
        return study

    def _tpe_ask(self, study, params: dict, fixed: dict) -> dict:
        import optuna
        base = study.sampler
        if fixed:
            study.sampler = optuna.samplers.PartialFixedSampler(fixed, base)
        trial = study.ask(study._tinydet_dists)
        study.sampler = base
        return dict(trial.params)

    def plan(self, k: int | None = None) -> list[JobSpec] | None:
        """Decide the next group of jobs and record them in the ledger."""
        space, st = self.space, self.state()
        k_arg = k
        k = k_arg or space["schedule"].get("group_size", 4)
        orphans = self._orphans(space, st)
        if orphans:  # work interrupted by a crash or a notebook interrupt: redo it first
            sig = Config(orphans[0][0]["config"]).data_signature()
            jobs = []
            for t, r, resume in orphans:
                cfg = Config(t["config"])
                if cfg.data_signature() == sig and len(jobs) < k:
                    stage = next(s for s in space["stages"] if s["name"] == t["stage"])
                    rungs = self._rungs(space, stage)
                    jobs.append(JobSpec(t["id"], cfg, self.exp / "trials" / t["id"], r, rungs[r], resume=resume))
            self.log(f"resuming {len(jobs)} interrupted job(s)")
            return jobs
        while True:
            stage = self.current_stage(space, st)
            if stage is None:
                return None
            rungs = self._rungs(space, stage)
            promos = self._promotions(space, stage, st)
            budget_left = stage["trials"] - len(st.stage_trials(stage["name"]))
            if not promos and budget_left <= 0:
                pending = [t for t in st.stage_trials(stage["name"]) if not t["failed"] and not t["rungs"]]
                fin = None if pending else self._finalize(space, stage, st)
                if fin is None:
                    self._close_stage(space, stage, st)
                    st = self.state()
                    continue
                promos = [fin]
            break

        k = k_arg or stage.get("group_size") or space["schedule"].get("group_size", 4)
        jobs: list[JobSpec] = []
        n_budget = max(0, stage["trials"] - len(st.stage_trials(stage["name"])))
        promo_first = n_budget == 0 or len(promos) >= k
        promos.sort(key=lambda tr: -(score(tr[0], tr[1] - 1) or -1e9))

        def add_promos(sig):
            for t, r in promos:
                cfg = Config(t["config"])
                if len(jobs) < k and cfg.data_signature() == sig and not any(j.trial_id == t["id"] for j in jobs):
                    self.ledger.append("promoted", trial=t["id"], to_rung=r, epochs=rungs[r])
                    jobs.append(JobSpec(t["id"], cfg, self.exp / "trials" / t["id"], r, rungs[r], resume=True))

        if promo_first and promos:
            sig = Config(promos[0][0]["config"]).data_signature()
            add_promos(sig)
            force = {kk: jobs[0].cfg[kk] for kk in DATA_KEYS}
            for cand, origin in self._new_trials(space, stage, st, min(k - len(jobs), n_budget), force):
                jobs.append(self._create(cand, origin, space, stage, st, rungs))
                st = self.state()
        elif n_budget > 0:
            # The first new trial samples its data parameters freely; promotions that share
            # them join the group, and the remaining new trials reuse them (lockstep).
            first = self._new_trials(space, stage, st, 1, None)
            if first:
                jobs.append(self._create(*first[0], space, stage, st, rungs))
                st = self.state()
                sig = jobs[0].cfg.data_signature()
                add_promos(sig)
                force = {kk: jobs[0].cfg[kk] for kk in DATA_KEYS}
                n_more = min(k - len(jobs), n_budget - 1)
                for cand, origin in self._new_trials(space, stage, st, n_more, force):
                    jobs.append(self._create(cand, origin, space, stage, st, rungs))
                    st = self.state()
            elif promos:
                add_promos(Config(promos[0][0]["config"]).data_signature())
        return jobs

    def _create(self, cand, origin, space, stage, st, rungs) -> JobSpec:
        cfg = self.base_config(space, st).override(cand)
        tid = f"t{st.next_id():04d}-{cfg.hash(n=6)}"
        tdir = self.exp / "trials" / tid
        tdir.mkdir(parents=True, exist_ok=True)
        cfg.save(tdir / "config.json")
        _, prof = self._check(space, cfg)
        self.ledger.append("trial_created", trial=tid, stage=stage["name"], params=cand,
                           origin=origin, config=cfg.to_dict(), profile=prof)
        return JobSpec(tid, cfg, tdir, 0, rungs[0])

    # ── running ────────────────────────────────────────────────────────────
    def step(self, k: int | None = None) -> bool:
        jobs = self.plan(k)
        if jobs is None:
            self.log("search finished: every stage is closed")
            return False
        if not jobs:
            self.log("nothing to run (all candidates infeasible?); check space.json")
            return False
        sched = self.space["schedule"]
        run_group(jobs, self.ctx, self.ledger, self.space["objective"], device=self.device,
                  eval_every=sched.get("eval_every", 5), report_thr=self.space.get("report_threshold", 0.3),
                  mlflow=self.mlflow, log=self.log)
        self.report(write=True)
        return True

    def run(self, max_groups: int | None = None, max_hours: float | None = None,
            k: int | None = None, dashboard: bool = True):
        import time
        t0, n = time.time(), 0
        while (max_groups is None or n < max_groups) and (max_hours is None or (time.time() - t0) / 3600 < max_hours):
            if not self.step(k):
                break
            n += 1
            if dashboard:
                from . import dashboard as dash
                dash.build(self.exp, self.space_path)
        self.log(f"ran {n} group(s) in {(time.time() - t0) / 60:.1f} min")

    # ── reporting ──────────────────────────────────────────────────────────
    def leaderboard(self, n: int = 20, stage: str | None = None):
        import pandas as pd
        rows = []
        for t in self.state().trials.values():
            if stage and t["stage"] != stage:
                continue
            r = top_rung(t)
            m = t["rungs"].get(r, {}).get("metrics", {})
            rows.append({"trial": t["id"], "stage": t["stage"], "origin": t["origin"], "rung": r,
                         "epochs": t["rungs"].get(r, {}).get("epochs"), "objective": score(t),
                         "zoom_ap": m.get("val_zoom/ap"), "dev_ap": m.get("val_dev/ap"),
                         "ece_cal": m.get("val_zoom/ece_cal"), "zoom_f1": m.get("val_zoom/f1_best"),
                         "full_f1": m.get("val_full/f1"), "fp_img": m.get("val_neg/fp_per_img"),
                         "mmacs": t["profile"].get("mmacs"), "pred_ms": t["profile"].get("pred_ms"),
                         "failed": t["failed"], **{f"p:{k}": v for k, v in t["params"].items()}})
        df = pd.DataFrame(rows)
        if df.empty:
            return df
        return df.sort_values(["rung", "objective"], ascending=[False, False]).head(n).reset_index(drop=True)

    def status(self):
        space, st = self.space, self.state()
        cur = self.current_stage(space, st)
        for s in space["stages"]:
            ts = st.stage_trials(s["name"])
            done = [t for t in ts if t["rungs"]]
            best = max((score(t) for t in done if score(t) is not None), default=None)
            flag = "closed" if s["name"] in st.closed else ("ACTIVE" if cur and s["name"] == cur["name"] else "waiting")
            self.log(f"{s['name']:<14} {flag:<8} trials {len(ts)}/{s['trials']}  "
                     f"best {best if best is None else round(best, 4)}")
        self.log(f"pinned: {self.pinned(space, st)}")
        self.log(f"infeasible skipped: {len(st.infeasible)}")

    def report(self, write: bool = True) -> dict:
        """Observed effect of each parameter (rung-0 objective), next to your priors."""
        space, st = self.space, self.state()
        out = {}
        for k, p in space["params"].items():
            ts = [t for t in st.trials.values() if t["stage"] == p.get("stage") and 0 in t["rungs"]
                  and t["rungs"][0].get("objective") is not None]
            if not ts:
                continue
            by = defaultdict(list)
            for t in ts:
                v = t["config"].get(k)
                if p["type"] != "categorical":
                    v = _bin(v, p)
                by[json.dumps(v)].append(t["rungs"][0]["objective"])
            stats = {v: {"n": len(xs), "mean": sum(xs) / len(xs), "best": max(xs)} for v, xs in by.items()}
            means = [s["mean"] for s in stats.values()]
            spread = max(means) - min(means) if len(means) > 1 else 0.0
            out[k] = {"stage": p.get("stage"), "prior_impact": p.get("impact"),
                      "prior_confidence": p.get("confidence"), "observed_spread": spread,
                      "values": stats}
        imp = self._importance(space, st)
        for k, v in imp.items():
            if k in out:
                out[k]["observed_importance"] = v
        if write:
            (self.exp / "observed.json").write_text(json.dumps(out, indent=2))
        return out

    def _importance(self, space, st) -> dict:
        try:
            import optuna
            from optuna.importance import PedAnovaImportanceEvaluator, get_param_importances
        except Exception:
            return {}
        res = {}
        for stage in space["stages"]:
            params = {k: p for k, p in space["params"].items() if p.get("stage") == stage["name"]}
            ts = [t for t in st.stage_trials(stage["name"]) if t["rungs"].get(0, {}).get("objective") is not None]
            if len(params) < 2 or len(ts) < 8:
                continue
            dists = {k: _distribution(p) for k, p in params.items()}
            study = optuna.create_study(direction="maximize")
            for t in ts:
                try:
                    study.add_trial(optuna.trial.create_trial(
                        params={k: t["config"][k] for k in params}, distributions=dists,
                        value=t["rungs"][0]["objective"]))
                except ValueError:
                    pass
            try:
                res.update(get_param_importances(study, evaluator=PedAnovaImportanceEvaluator()))
            except Exception:
                pass
        return res

    def manual(self, overrides: dict | None = None, epochs: int = 60, schedule: str = "cosine",
               note: str = "") -> dict:
        """Train one config outside the staged search (stage "manual"), logged like any trial.

        Uses the current pinned/base config plus ``overrides``; cosine schedule by default.
        """
        space, st = self.space, self.state()
        overrides = dict(overrides or {})
        cfg = self.base_config(space, st).override({"optim.schedule": schedule, **overrides})
        err, prof = self._check(space, cfg)
        if err and M.check_config(cfg):
            raise ValueError(err)
        tid = f"t{st.next_id():04d}-{cfg.hash(n=6)}"
        tdir = self.exp / "trials" / tid
        tdir.mkdir(parents=True, exist_ok=True)
        cfg.save(tdir / "config.json")
        self.ledger.append("trial_created", trial=tid, stage="manual", params=overrides,
                           origin="manual", config=cfg.to_dict(), profile=prof, note=note)
        res = run_group([JobSpec(tid, cfg, tdir, 0, epochs)], self.ctx, self.ledger, space["objective"],
                        device=self.device, eval_every=space["schedule"].get("eval_every", 5),
                        report_thr=space.get("report_threshold", 0.3), mlflow=self.mlflow, log=self.log)
        return res.get(tid, {})

    def reevaluate(self, trial_ids: list[str] | None = None, only_missing: bool = True):
        """Re-score saved rung checkpoints with the current evaluation code.

        Use after the evaluation gains a metric the objective needs (trials scored before
        then have objective None and drop out of promotion). Appends new rung_done events;
        the latest one per (trial, rung) wins.
        """
        import torch
        from . import data as D
        from . import export as X
        space, st = self.space, self.state()
        device = self.device or torch.device("cuda" if torch.cuda.is_available() else "cpu")
        n = 0
        for t in st.trials.values():
            if trial_ids and t["id"] not in trial_ids:
                continue
            for r, row in sorted(t["rungs"].items()):
                if only_missing and row.get("objective") is not None:
                    continue
                tdir = self.exp / "trials" / t["id"]
                if not (tdir / f"rung{r}.pt").exists():
                    continue
                model, cfg, _ = X.load_trial(tdir, r)
                model = model.to(device)
                prep = M.InputPrep(cfg, device)
                metrics = {}
                for name in D.EVAL_SPLITS:
                    m = E.evaluate({"m": (model, prep)}, self.ctx.eval_loader(name, cfg), cfg, device,
                                   space.get("report_threshold", 0.3))["m"]
                    metrics.update({f"{name}/{k}": v for k, v in m.items()})
                obj = E.objective(metrics, space["objective"])
                self.ledger.append("rung_done", trial=t["id"], rung=r, epochs=row.get("epochs"),
                                   trained_epochs=row.get("trained_epochs"), objective=obj,
                                   metrics=metrics, reevaluated=True)
                self.log(f"  re-evaluated {t['id']} rung {r}: objective={obj}")
                n += 1
        self.log(f"re-evaluated {n} checkpoint(s)")

    def note(self, text: str):
        self.ledger.append("note", text=text)


def _bin(v, p) -> str:
    """Numeric values are reported in 4 bins over the range (log bins if log=True)."""
    lo, hi = p["low"], p["high"]
    tf = math.log if p.get("log") else (lambda x: x)
    q = (tf(v) - tf(lo)) / max(1e-12, tf(hi) - tf(lo))
    b = min(3, max(0, int(q * 4)))
    edges = [lo + (hi - lo) * i / 4 for i in range(5)] if not p.get("log") else \
        [math.exp(tf(lo) + (tf(hi) - tf(lo)) * i / 4) for i in range(5)]
    return f"{edges[b]:.3g}-{edges[b + 1]:.3g}"
