# tinydet

Tiny centroid-heatmap object detectors for microcontrollers, plus the harness that searches
their hyperparameters. Built for a stride-8 MobileNetV2 detector running INT8 on an ESP32-S3.

## Quick start

```bash
cd models
pip install -r requirements.txt
python tests/smoke_test.py            # self-check on synthetic data, no COCO needed (~2-5 min)
python tests/smoke_test.py --export   # also checks ONNX -> INT8 TFLite -> firmware metadata
jupyter lab experiments.ipynb
```

The notebook builds the data index and splits, previews augmentations, runs the search,
shows the dashboard and exports the best trial to the firmware.

## Layout

| Module | Does |
| --- | --- |
| `config.py` | Flat dotted-key config (`model.width`, `target.sigma`, ...) with defaults, hashing and the data signature |
| `data.py` | COCO index cache, fixed splits (`val_zoom`, `val_dev`, `val_full`, `val_neg`), datasets, augmentation pipeline |
| `camera.py` | Simulation of the ESP32-S3 / OV2640 pipeline: noise + auto-gain, exposure, white balance, ISP sharpening, lens, motion, rolling shutter, RGB565 |
| `paste.py` | Copy-paste of COCO ball cutouts (masks) at the car's working sizes, with floor placement and contact shadows |
| `targets.py` | GPU target rendering: heatmap forms, sigma, ignore regions for tiny / crowd / clipped balls |
| `models.py` | timm MobileNetV2 backbones cut at stride 8, five heads, optional TAP and context branch, profiler |
| `losses.py` | CenterNet focal, BCE, sigmoid focal |
| `evaluate.py` | Firmware-faithful peak decoding, greedy matching, AP / F1 / recall by size / FP per image, confidence calibration (ECE, Brier, Platt) |
| `train.py` | Lockstep multi-model trainer with warmup-stable-decay rungs and resumable checkpoints |
| `search.py` | Staged search: priors -> Optuna TPE, asynchronous successive halving, constraints |
| `tracking.py` | Experiment folders, JSONL ledger, optional MLflow mirror |
| `dashboard.py` | Static Plotly dashboard built from the ledger |
| `export.py` | Conv+BN fusion, ONNX, INT8 TFLite, INT8 evaluation, firmware metadata |
| `viz.py` | Notebook previews |

## The search's memory

**`search/space.json`** (you edit it; re-read before every group)

| Field | Meaning |
| --- | --- |
| `objective.terms` | Weighted sum of metrics to maximise, e.g. `val_zoom/ap` |
| `constraints` | `max_pred_ms`, `max_peak_kb`, `max_params`; violating configs are skipped before training |
| `latency_model` | `pred_ms = intercept_ms + ms_per_mmac * MMACs`; calibrate with measured `invoke()` times |
| `schedule` | `rungs` (epochs), `eta` (keep top 1/eta), `group_size` (trials trained in lockstep), `eval_every` |
| `base` | Config overrides for every trial |
| `fixed` | Values you pin by hand; they beat stage winners |
| `params.<key>` | `type`, `choices` or `low`/`high`/`log`, `prior`, `impact` (0-1, how much you think it matters), `confidence` (0-1, how sure you are of the prior's favourite), `stage`, `evidence` |
| `stages[]` | `name`, `trials` budget, `startup` prior draws before TPE, optional `seeds`, `eta`, `rungs` |

`confidence` is used directly: during the prior phase, a parameter takes its prior's favourite
value with probability `confidence`, otherwise it is drawn from the prior weights.

**`experiments/<name>/ledger.jsonl`** (append-only; the search writes it)

Events: `experiment_started`, `trial_created`, `trial_infeasible`, `promoted`, `rung_done`,
`trial_failed`, `stage_closed`, `note`. Each trial also has `trials/<id>/` with its config,
per-epoch `metrics.jsonl`, `stable.pt` (resume point), `rung<k>.pt` weights and `eval_r<k>.json`.

## Why lockstep groups

These models are tiny, so training is bound by JPEG decoding and augmentation on the CPU.
Trials that share the data settings (`data.*` and `optim.batch_size`) train on the same batches:
one loader feeds up to `group_size` models, each with its own targets, loss, optimiser,
normalisation and EMA. Targets are rendered on the GPU from raw boxes to make this possible.

## Why warmup-stable-decay rungs

Successive halving needs a fair score after 15 epochs and the option to continue to 40 and 120
without restarting. A trial trains at a constant LR to its rung, saves `stable.pt`, then runs a
short decay branch and is scored there. If promoted, it resumes from `stable.pt`.

## Confidence

SLAM consumes the score as a probability, so every evaluation reports calibration over the
detections: ECE, Brier and log loss, raw and after Platt scaling fitted on half the images and
scored on the other half (`ece_cal`). `export.apply_platt` folds the fit into the last conv
(exact, zero cost), so the firmware's sigmoid outputs calibrated probabilities and
`conf_threshold` means "probability this is a ball". `val_zoom/ece_cal` is in the default
objective. Objectives are recomputed from stored metrics, so editing them re-scores history;
`Search.reevaluate()` re-scores old checkpoints when a new metric is needed.

## Data pipeline order (training)

geometric crop -> copy-paste -> camera geometry (lens, rolling-shutter shear) -> generic
photometric -> camera sensor / ISP / RGB565. `val_dev` runs half of the `val_zoom` windows
through a fixed, seeded camera model as a proxy for the real camera until real frames exist.

## Dashboard and MLflow

`tinydet.dashboard.build(exp_dir, space_path)` writes `dashboard.html` (rebuilt after every
group). `Search(..., mlflow=True)` also logs each rung to `experiments/<name>/mlruns`; browse it
with `mlflow ui --backend-store-uri experiments/<name>/mlruns`.

## Firmware note

`model.context = true` adds a `RESIZE_NEAREST_NEIGHBOR` op. `firmware/components/fomo/fomo.cpp`
registers it (resolver size 21).
