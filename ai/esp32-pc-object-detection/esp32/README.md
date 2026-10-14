# ESP32 firmware: FOMO detection in the browser

Runs the int8 FOMO model exported by `../fomo.ipynb` on camera frames and
serves the result to a web page: the exact pixels the model saw, a per-cell
heatmap, the detections, per-stage timings and live memory use.

Default target: **Freenove ESP32-S3 WROOM CAM** (ESP32-S3, 8 MB flash, 8 MB
octal PSRAM). The AI-Thinker **ESP32-CAM** still builds too.

## Build and flash

```sh
# from the nix dev shell (../flake.nix)
export WIFI_SSID=yournetwork WIFI_PASSWORD=yourpassword
cargo run --release            # builds, flashes, opens the serial monitor
```

The log prints `Open http://<ip>/ in a browser`.

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
- `model_metadata.json`: needs `class_names` (background first) and
  `normalization.mean` / `normalization.std` (one value per input channel).
  Optional: `output_activation` (`"sigmoid"`, `"softmax"` or `"none"`) and
  `conf_threshold`.

Input width and height (square or not), channel count (gray or RGB) and grid
size are read from the model at boot, so nothing in the firmware changes when you retrain at a new
resolution. Without `output_activation` the decoder guesses: a background
channel means softmax, no background channel means sigmoid heatmaps.

## Layout

| Path | Runs on | Does |
|---|---|---|
| `components/fomo/fomo.cpp` | core 1 | camera, crop/resize, int8 quantise, TFLite Micro `Invoke()` |
| `src/detect.rs` | core 1 | output tensor → probabilities → 3×3 peak detections |
| `src/inference.rs` | core 1 | the capture → infer → decode loop |
| `src/server.rs` | core 0 | HTTP API and the page in `web/index.html` |
| `build.rs` | your PC | turns `model/model_metadata.json` into Rust constants |

`src/detect.rs` has unit tests that run on your PC:

```sh
rustc --edition 2021 --test src/detect.rs -o /tmp/detect && /tmp/detect
```

## HTTP API

| Endpoint | Returns |
|---|---|
| `GET /` | the web page |
| `GET /api/info` | model shapes, quantisation, arena size and placement, camera |
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
