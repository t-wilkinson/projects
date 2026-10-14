# ESP32 firmware: FOMO detection in the browser

Runs the int8 FOMO model exported by `../fomo.ipynb` on camera frames and
serves the result to a web page: the exact pixels the model saw, a per-cell
heatmap, the detections, per-stage timings and live memory use.

Default target: **Freenove ESP32-S3 WROOM CAM** (ESP32-S3, 16 MB flash, 8 MB
octal PSRAM). The AI-Thinker **ESP32-CAM** still builds too.

## Build and flash

```sh
# from the nix dev shell (../flake.nix)
export WIFI_SSID=yournetwork WIFI_PASSWORD=yourpassword
cargo run --release            # builds, flashes, opens the serial monitor
```

The log prints `Open http://<ip>/ in a browser`.

Run it from this directory: the runner passes `partitions.csv` to espflash,
which gives the app 3 MB. Without it espflash uses a default table with a
1 MB app slot and refuses the ~1.5 MB firmware (`espflash::image_too_big`).
To flash by hand, pass the same flag:

```sh
espflash flash --monitor --partition-table partitions.csv target/xtensa-esp32s3-espidf/release/esp32
```

The dev shell targets the S3 (`MCU=esp32s3`) and points bindgen at
Espressif's libclang; upstream clang fails with
`unknown target triple 'xtensa'`. For the original ESP32-CAM:

```sh
MCU=esp32 cargo run --release --target xtensa-esp32-espidf
```

If the S3 boot log says `PSRAM ID read error`, your module has quad PSRAM:
change `CONFIG_SPIRAM_MODE_OCT` to `CONFIG_SPIRAM_MODE_QUAD` in
`sdkconfig.defaults.esp32s3`.

## Updating the model

The notebook's export cell copies two files into `model/`:

- `fomo_model_int8.tflite`: int8 input and output, NHWC.
- `model_metadata.json`: what the firmware is built around. The notebook reads
  the tensor shapes and quantisation back from the `.tflite`, and the training
  settings from the checkpoint the weights came from.

```jsonc
{
  "format_version": 2,
  "model_size_bytes": 114408,
  "input":  { "width": 120, "height": 120, "channels": 3, "dtype": "int8", "layout": "NHWC",
              "scale": 0.0187, "zero_point": -14 },
  "output": { "width": 15, "height": 15, "channels": 1, "dtype": "int8", "layout": "NHWC",
              "scale": 0.068, "zero_point": 27 },     // width x height is the grid
  "grid_stride": 8,                                   // input pixels per grid cell
  "class_names": ["background", "sports_ball"],       // background first
  "output_activation": "sigmoid",                     // or "softmax", "none"
  "normalization": { "mean": [0.485, 0.456, 0.406], "std": [0.229, 0.224, 0.225] },
  "conf_threshold": 0.3,                              // starting value; the page can change it
  "training": { "INPUT_SIZE": 120, "LEARNING_RATE": 0.002, "epoch": 143, "f1": 0.61, ... }
}
```

`build.rs` parses the `.tflite` as well and fails the build with a list of
mismatches if the JSON describes a different model (for example a `.tflite`
converted from a stale ONNX file, or one copied in without re-running the
export cell). Then it generates Rust constants (`src/model.rs` re-exports
them):

| Constant | From | Used for |
|---|---|---|
| `INPUT_W/H/C`, `GRID_W/H`, `OUTPUT_C`, scales, zero points | `input`, `output` | checked against the interpreter's tensors at boot |
| `CAMERA_W/H`, `CAMERA_FRAME` | smallest camera frame covering the input | camera capture size |
| `NORM_MEAN`, `NORM_STD` | `normalization` | pixel → int8 lookup table |
| `CLASS_NAMES`, `OUTPUT_ACTIVATION` | `class_names`, `output_activation` | decoding, labels |
| `CONF_THRESHOLD` | `conf_threshold` | initial detection threshold |
| `METADATA_JSON` | the whole file | `GET /api/model` |

So retraining at a new resolution, in grayscale or with other classes only
needs the export cell and a rebuild. At boot the firmware checks the
interpreter's tensors against the constants and stops with both shapes in the
log if they differ.

## Layout

| Path | Runs on | Does |
|---|---|---|
| `components/fomo/fomo.cpp` | core 1 | camera, crop/resize, int8 quantise, TFLite Micro `Invoke()` |
| `src/detect.rs` | core 1 | output tensor → probabilities → 3×3 peak detections |
| `src/inference.rs` | core 1 | the capture → infer → decode loop |
| `src/server.rs` | core 0 | HTTP API and the page in `web/index.html` |
| `build.rs` | your PC | checks `model/model_metadata.json` against the `.tflite`, turns it into Rust constants |

`src/detect.rs` has unit tests that run on your PC:

```sh
rustc --edition 2021 --test src/detect.rs -o /tmp/detect && /tmp/detect
```

## HTTP API

| Endpoint | Returns |
|---|---|
| `GET /` | the web page |
| `GET /api/info` | model shapes, quantisation, arena size and placement, camera |
| `GET /api/model` | the `model_metadata.json` the firmware was built with, training settings included |
| `GET /api/frame` | `[u32 LE json length][json][image H×W×C][scores gridH×gridW×classes]` |
| `GET /api/settings?threshold=0.4&vflip=1&hmirror=0` | current settings |

To work on the page without reflashing, open `web/index.html` from disk with
`?device=http://<esp32-ip>`.

## Where things live in memory

| Thing | Where | Why |
|---|---|---|
| Model weights (`.tflite`) | flash, read through the cache | read-only, so no RAM needed; 16-byte aligned so Xtensa can load 32-bit values |
| Tensor arena | internal SRAM if it fits, else PSRAM | activations are read and written every layer; internal SRAM has no cache misses |
| Camera frame buffers (×2) | PSRAM | large, filled by DMA while the CPU works on the other one |
| WiFi / lwIP buffers | PSRAM (`CONFIG_SPIRAM_TRY_ALLOCATE_WIFI_LWIP`) | leaves internal SRAM for the arena |
| Task stacks | internal SRAM | FreeRTOS default |

At boot the pipeline builds the interpreter in a 1 MB PSRAM arena, reads
`arena_used_bytes()`, and moves the arena into internal SRAM if that leaves at
least 64 KB free. The serial log and `/api/info` say where it ended up.
