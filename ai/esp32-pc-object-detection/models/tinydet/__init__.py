"""tinydet: tiny centroid-heatmap object detectors for microcontrollers.

Layout
    config     flat dotted-key config with defaults, hashing and the data signature
    camera     simulation of the ESP32-S3 / OV2640 image pipeline (augmentation + val_dev)
    paste      copy-paste of COCO ball cutouts onto training images
    data       COCO index cache, fixed splits, datasets, augmentation pipelines
    targets    GPU target rendering (heatmap forms, sigma, ignore masks)
    models     backbone registry (timm / torchvision MobileNetV2), heads, profiler
    losses     CenterNet focal, BCE, sigmoid focal
    evaluate   firmware-faithful peak decoding, greedy matching, PR/AP/F1, recall by size
    train      lockstep multi-model trainer with warmup-stable-decay rungs
    tracking   experiment folders, JSONL ledger, optional MLflow mirror
    search     prior-guided search planner (TPE + asynchronous successive halving)
    dashboard  static Plotly HTML dashboard built from the ledger
    export     Conv+BN fusion, ONNX, INT8 TFLite, TFLite evaluation, firmware metadata
    viz        augmentation and prediction previews

In a notebook, call ``tinydet.reload()`` after editing files (or use ``%autoreload 2``).
"""

import importlib
import sys

__version__ = "0.1.0"

# Dependency order: a module is reloaded after everything it imports.
_MODULES = [
    "config", "tracking", "targets", "camera", "paste", "data", "models", "losses",
    "evaluate", "train", "search", "dashboard", "export", "viz",
]


def reload(verbose: bool = True):
    """Reload every tinydet submodule in dependency order.

    Lets you edit the package files and pick up the changes without restarting the
    kernel. Objects created before the reload (models, datasets) keep their old
    classes, so rebuild them after reloading.
    """
    pkg = sys.modules[__name__]
    reloaded = []
    for name in _MODULES:
        full = f"{__name__}.{name}"
        if full in sys.modules:
            setattr(pkg, name, importlib.reload(sys.modules[full]))
            reloaded.append(name)
    if verbose:
        print(f"tinydet: reloaded {', '.join(reloaded) or 'nothing (no submodule imported yet)'}")
    return pkg


def __getattr__(name):
    # Lazy submodule import: `tinydet.search` imports tinydet/search.py on first use,
    # so `import tinydet` stays cheap and doesn't need TensorFlow etc.
    if name in _MODULES:
        module = importlib.import_module(f"{__name__}.{name}")
        setattr(sys.modules[__name__], name, module)
        return module
    raise AttributeError(f"module 'tinydet' has no attribute {name!r}")
