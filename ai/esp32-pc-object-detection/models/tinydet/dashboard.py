"""Static experiment dashboard (one self-contained HTML file, Plotly from its CDN).

Built from the ledger, each trial's metrics.jsonl, observed.json and space.json, so it
can be rebuilt at any time and opened in a browser while the search runs:

    tinydet.dashboard.build("experiments/ball-v1", "search/space.json")

Panels: stage progress, leaderboard, accuracy vs predicted latency (the Pareto front),
successive-halving ladder, learning curves, per-parameter effects, prior vs observed
importance, recall by ball size for the leaders.
"""

from __future__ import annotations

import html
import json
from pathlib import Path

from . import tracking
from .search import State, load_space, score, top_rung

SERIES = ["#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948"]
SYMBOLS = ["circle", "square", "diamond", "triangle-up", "cross", "x", "star", "hexagon"]
INK, INK2, GRID, SURFACE = "#0b0b0b", "#52514e", "#e7e6e2", "#fcfcfb"


def _fmt(v):
    return f"{v:.3g}" if isinstance(v, float) else str(v)


def _layout(fig, title: str, height: int = 380):
    fig.update_layout(
        title=dict(text=title, x=0, font=dict(size=15, color=INK)),
        height=height, margin=dict(l=56, r=20, t=48, b=48), paper_bgcolor=SURFACE,
        plot_bgcolor=SURFACE, font=dict(family="system-ui, -apple-system, Segoe UI, sans-serif",
                                        size=12, color=INK2),
        legend=dict(orientation="h", y=-0.18, x=0), hoverlabel=dict(bgcolor="white"),
    )
    fig.update_xaxes(gridcolor=GRID, zeroline=False, linecolor=GRID)
    fig.update_yaxes(gridcolor=GRID, zeroline=False, linecolor=GRID)
    return fig


def build(exp_dir: str | Path, space_path: str | Path, out: str | Path | None = None,
          top: int = 25, offline: bool = False) -> Path:
    """Write the dashboard. ``offline=True`` embeds plotly.js (~4 MB) instead of the CDN link."""
    import plotly.graph_objects as go
    from plotly.subplots import make_subplots

    exp = Path(exp_dir)
    space = load_space(space_path)
    st = State(tracking.Ledger(exp / "ledger.jsonl").read(), space["objective"])
    trials = list(st.trials.values())
    stages = [s["name"] for s in space["stages"]]
    if any(t["stage"] == "manual" for t in trials):
        stages.append("manual")
    color = {s: SERIES[i % len(SERIES)] for i, s in enumerate(stages)}
    symbol = {s: SYMBOLS[i % len(SYMBOLS)] for i, s in enumerate(stages)}
    observed = {}
    if (exp / "observed.json").exists():
        observed = json.loads((exp / "observed.json").read_text())
    figs: list[tuple[str, object]] = []

    def hover(t):
        p = "<br>".join(f"{k}={_fmt(v)}" for k, v in t["params"].items()) or "(base config)"
        return f"<b>{t['id']}</b> ({t['stage']}, {t['origin']})<br>{p}"

    # 1. Accuracy vs predicted latency
    fig = go.Figure()
    for s in stages:
        ts = [t for t in trials if t["stage"] == s and score(t) is not None]
        if not ts:
            continue
        fig.add_trace(go.Scatter(
            x=[t["profile"].get("pred_ms") for t in ts], y=[score(t) for t in ts], mode="markers",
            name=s, marker=dict(size=[8 + 4 * top_rung(t) for t in ts], color=color[s], symbol=symbol[s],
                                line=dict(color=SURFACE, width=2)),
            text=[hover(t) + f"<br>rung {top_rung(t)}" for t in ts], hovertemplate="%{text}<br>%{x} ms, objective %{y:.4f}<extra></extra>"))
    lim = space.get("constraints", {}).get("max_pred_ms")
    if lim:
        fig.add_vline(x=lim, line=dict(color=INK2, dash="dot", width=1),
                      annotation_text=f"budget {lim} ms", annotation_position="top left")
    cal = "calibrated" if space.get("latency_model", {}).get("calibrated") else "uncalibrated estimate"
    fig.update_xaxes(title=f"predicted invoke() ms ({cal})")
    fig.update_yaxes(title="objective (top rung reached)")
    figs.append(("pareto", _layout(fig, "Accuracy vs latency (marker size = rung reached)")))

    # 2. Successive-halving ladder
    fig = go.Figure()
    for t in trials:
        if not t["rungs"]:
            continue
        rs = sorted(t["rungs"])
        fig.add_trace(go.Scatter(
            x=[t["rungs"][r].get("trained_epochs", t["rungs"][r].get("epochs")) for r in rs],
            y=[t["rungs"][r].get("objective") for r in rs], mode="lines+markers",
            line=dict(color=color.get(t["stage"], INK2), width=2), marker=dict(size=8),
            name=t["id"], legendgroup=t["stage"], showlegend=False,
            text=[hover(t)] * len(rs), hovertemplate="%{text}<br>%{x} epochs: %{y:.4f}<extra></extra>"))
    fig.update_xaxes(title="epochs trained (incl. decay branch)", type="log")
    fig.update_yaxes(title="objective")
    figs.append(("ladder", _layout(fig, "Successive halving: each line is one trial, coloured by stage")))

    # 3. Learning curves (loss and quick val AP), most recent 8 trials (one colour each)
    recent = [t for t in trials if (exp / "trials" / t["id"] / "metrics.jsonl").exists()][-8:]
    fig = make_subplots(rows=1, cols=2, subplot_titles=("training loss", "quick val_zoom AP (subset, EMA)"))
    for i, t in enumerate(recent):
        rows = tracking.read_jsonl(exp / "trials" / t["id"] / "metrics.jsonl")
        last = {}
        for r in rows:
            last[r["epoch"]] = r
        rows = [last[e] for e in sorted(last)]
        c = SERIES[i % len(SERIES)]
        fig.add_trace(go.Scatter(x=[r["epoch"] for r in rows], y=[r.get("loss") for r in rows], mode="lines",
                                 line=dict(color=c, width=2), name=t["id"], legendgroup=t["id"]), 1, 1)
        q = [r for r in rows if "quick/ap" in r]
        fig.add_trace(go.Scatter(x=[r["epoch"] for r in q], y=[r["quick/ap"] for r in q], mode="lines+markers",
                                 line=dict(color=c, width=2), marker=dict(size=6), name=t["id"],
                                 legendgroup=t["id"], showlegend=False), 1, 2)
    fig.update_xaxes(title="epoch")
    figs.append(("curves", _layout(fig, "Learning curves (last 8 trials)", 400)))

    # 4. Per-parameter effects (rung-0 objective by value), one panel per searched parameter
    params = [(k, p) for k, p in space["params"].items()
              if any(t["stage"] == p.get("stage") and 0 in t["rungs"] for t in trials)]
    if params:
        cols = 3
        nrows = (len(params) + cols - 1) // cols
        fig = make_subplots(rows=nrows, cols=cols, subplot_titles=[k for k, _ in params],
                            vertical_spacing=0.12 / max(1, nrows / 3))
        for i, (k, p) in enumerate(params):
            ts = [t for t in trials if t["stage"] == p.get("stage") and t["rungs"].get(0, {}).get("objective") is not None]
            xs = [str(t["config"].get(k)) if p["type"] == "categorical" else t["config"].get(k) for t in ts]
            fig.add_trace(go.Box(x=xs, y=[t["rungs"][0]["objective"] for t in ts], boxpoints="all", jitter=0.4,
                                 pointpos=0, marker=dict(color=color.get(p.get("stage"), SERIES[0]), size=7),
                                 line=dict(color=INK2, width=1), fillcolor="rgba(0,0,0,0)", showlegend=False,
                                 text=[t["id"] for t in ts], hovertemplate="%{text}<br>%{x}: %{y:.4f}<extra></extra>")
                          if p["type"] == "categorical" else
                          go.Scatter(x=xs, y=[t["rungs"][0]["objective"] for t in ts], mode="markers",
                                     marker=dict(color=color.get(p.get("stage"), SERIES[0]), size=8),
                                     showlegend=False, text=[t["id"] for t in ts],
                                     hovertemplate="%{text}<br>%{x:.3g}: %{y:.4f}<extra></extra>"),
                          row=i // cols + 1, col=i % cols + 1)
            if p.get("log"):
                fig.update_xaxes(type="log", row=i // cols + 1, col=i % cols + 1)
        figs.append(("effects", _layout(fig, "Parameter effects on the rung-0 objective", 280 * nrows + 60)))

    # 5. Prior vs observed importance
    if observed:
        ks = list(observed)
        obs = [observed[k].get("observed_importance", observed[k].get("observed_spread", 0)) for k in ks]
        mx = max(obs) or 1.0
        fig = go.Figure([
            go.Bar(y=ks, x=[observed[k].get("prior_impact") or 0 for k in ks], name="your prior impact (0-1)",
                   orientation="h", marker=dict(color=SERIES[0])),
            go.Bar(y=ks, x=[o / mx for o in obs], name="observed importance (scaled to max = 1)",
                   orientation="h", marker=dict(color=SERIES[1])),
        ])
        fig.update_layout(barmode="group", bargap=0.3, bargroupgap=0.1)
        fig.update_xaxes(title="0 = no effect, 1 = largest", range=[0, 1.05])
        figs.append(("beliefs", _layout(fig, "Beliefs check: where the priors were right or wrong", 120 + 36 * len(ks))))

    # 6. Recall by size for the leaders
    leaders = sorted([t for t in trials if score(t) is not None], key=lambda t: (-top_rung(t), -(score(t) or 0)))[:5]
    if leaders:
        buckets = ["0-4", "4-8", "8-16", "16-32", "32+"]
        fig = go.Figure()
        for i, t in enumerate(leaders):
            m = t["rungs"][top_rung(t)]["metrics"]
            fig.add_trace(go.Bar(x=buckets, y=[m.get(f"val_zoom/recall_{b}") for b in buckets], name=t["id"],
                                 marker=dict(color=SERIES[i])))
        fig.update_layout(barmode="group", bargap=0.25, bargroupgap=0.08)
        fig.update_xaxes(title="ball size (min side, input px)")
        fig.update_yaxes(title=f"recall @ {space.get('report_threshold', 0.3)}", range=[0, 1])
        figs.append(("sizes", _layout(fig, "Recall by ball size on val_zoom (top 5 trials)")))

    # 7. Confidence calibration: reliability diagrams for the leaders (val_zoom)
    if leaders and any(t["rungs"][top_rung(t)]["metrics"].get("val_zoom/reliability") for t in leaders[:3]):
        fig = make_subplots(rows=1, cols=2, subplot_titles=("raw scores", "after Platt scaling (held-out half)"))
        for col in (1, 2):
            fig.add_trace(go.Scatter(x=[0, 1], y=[0, 1], mode="lines", line=dict(color=INK2, dash="dot", width=1),
                                     showlegend=False, hoverinfo="skip"), 1, col)
        for i, t in enumerate(leaders[:3]):
            m = t["rungs"][top_rung(t)]["metrics"]
            for col, key, ece_key in ((1, "val_zoom/reliability", "val_zoom/ece"),
                                      (2, "val_zoom/reliability_cal", "val_zoom/ece_cal")):
                rel = m.get(key)
                if not rel:
                    continue
                pts = [(c, a, n) for c, a, n in zip(rel["conf"], rel["acc"], rel["n"]) if n]
                fig.add_trace(go.Scatter(
                    x=[p[0] for p in pts], y=[p[1] for p in pts], mode="lines+markers",
                    marker=dict(size=[6 + 10 * (p[2] / max(q[2] for q in pts)) ** 0.5 for p in pts], color=SERIES[i],
                                line=dict(color=SURFACE, width=2)),
                    line=dict(color=SERIES[i], width=2), legendgroup=t["id"], showlegend=col == 1,
                    name=f"{t['id']}  ECE {m.get('val_zoom/ece', float('nan')):.3f} raw -> "
                         f"{m.get('val_zoom/ece_cal', float('nan')):.3f} calibrated",
                    text=[f"{p[2]} detections" for p in pts],
                    hovertemplate="score %{x:.2f} -> %{y:.2f} real<br>%{text}<extra></extra>"), 1, col)
        fig.update_xaxes(title="predicted confidence", range=[0, 1])
        fig.update_yaxes(title="fraction that are real balls", range=[0, 1])
        figs.append(("calibration", _layout(fig, "Confidence calibration on val_zoom: on the diagonal = score means probability", 420)))

    # Tables
    stage_rows = []
    for s in space["stages"] + ([{"name": "manual", "trials": "-"}] if "manual" in stages else []):
        ts = st.stage_trials(s["name"])
        best = max((score(t) for t in ts if score(t) is not None), default=None)
        closed = st.closed.get(s["name"])
        current = next((x for x in stages if x not in st.closed and x != "manual"), None)
        state = "closed" if closed else ("active" if s["name"] == current else "waiting")
        stage_rows.append([s["name"], state, f"{len(ts)}/{s['trials']}",
                           "" if best is None else f"{best:.4f}",
                           (closed or {}).get("winner") or "",
                           html.escape(", ".join(f"{k}={_fmt(v)}" for k, v in (closed or {}).get("fixed", {}).items()))])
    lead_rows = []
    ranked = sorted([t for t in trials if score(t) is not None], key=lambda t: (-top_rung(t), -(score(t) or 0)))[:top]
    for t in ranked:
        m = t["rungs"][top_rung(t)]["metrics"]
        f = lambda v: "" if v is None else f"{v:.3f}"
        lead_rows.append([t["id"], t["stage"], str(top_rung(t)), f(score(t)), f(m.get("val_zoom/ap")),
                          f(m.get("val_dev/ap")), f(m.get("val_zoom/ece_cal")),
                          f(m.get("val_zoom/f1_best")), f(m.get("val_full/f1")), f(m.get("val_neg/fp_per_img")),
                          str(t["profile"].get("mmacs", "")), str(t["profile"].get("pred_ms", "")),
                          html.escape(", ".join(f"{k}={_fmt(v)}" for k, v in t["params"].items()))])

    def table(head, rows):
        th = "".join(f"<th>{h}</th>" for h in head)
        trs = "".join("<tr>" + "".join(f"<td>{c}</td>" for c in r) + "</tr>" for r in rows)
        return f"<table><thead><tr>{th}</tr></thead><tbody>{trs}</tbody></table>"

    parts, first = [], True
    for _, f in figs:
        parts.append(f.to_html(full_html=False, include_plotlyjs=(True if offline else "cdn") if first else False,
                               config={"displaylogo": False, "responsive": True}))
        first = False
    n_inf = len(st.infeasible)
    n_fail = sum(t["failed"] for t in trials)
    doc = f"""<!DOCTYPE html><html><head><meta charset="utf-8"><title>{html.escape(space.get('name', 'search'))} dashboard</title>
<style>
body{{background:{SURFACE};color:{INK};font:14px system-ui,-apple-system,Segoe UI,sans-serif;margin:24px auto;max-width:1200px;padding:0 16px}}
h1{{font-size:22px;margin:0 0 4px}} .sub{{color:{INK2};margin-bottom:20px}}
table{{border-collapse:collapse;width:100%;margin:8px 0 28px;font-size:12.5px}}
th{{text-align:left;color:{INK2};font-weight:600;border-bottom:1px solid {GRID};padding:6px 8px}}
td{{border-bottom:1px solid {GRID};padding:5px 8px;vertical-align:top}} tr:hover td{{background:#f3f2ef}}
.card{{margin-bottom:28px}}
</style></head><body>
<h1>{html.escape(space.get('name', 'search'))}</h1>
<div class="sub">{len(trials)} trials · {n_fail} failed · {n_inf} infeasible configs skipped · built {tracking.now()}</div>
<h2 style="font-size:16px">Stages</h2>
{table(["stage", "state", "trials", "best objective", "winner", "pinned for later stages"], stage_rows)}
<h2 style="font-size:16px">Leaderboard (highest rung first)</h2>
{table(["trial", "stage", "rung", "objective", "zoom AP", "dev AP", "ECE cal", "zoom F1*", "full F1", "FP/img", "MMACs", "pred ms", "searched params"], lead_rows)}
{''.join(f'<div class="card">{p}</div>' for p in parts)}
<p class="sub">zoom F1* = best-threshold F1. dev AP = val_zoom windows through the simulated camera. ECE cal = calibration error after Platt scaling (lower is better). Objective terms: {html.escape(json.dumps(space['objective'].get('terms')))}</p>
</body></html>"""
    out = Path(out) if out else exp / "dashboard.html"
    out.write_text(doc)
    return out
