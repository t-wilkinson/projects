// ─────────────────────────────────────────────────────────────────────────────
// fomo.h — Minimal C API for the FOMO detection pipeline
//
// Camera init + capture + preprocess + TFLite inference + output parsing
// all live behind these two function calls.  Rust only sees detections.
// ─────────────────────────────────────────────────────────────────────────────
#pragma once

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// ── Detection result (C-ABI, packed for FFI) ─────────────────────────────────
typedef struct {
  uint8_t class_id; // 1–5 (0 = background, never returned)
  uint8_t grid_x;
  uint8_t grid_y;
  uint8_t _pad;     // alignment
  uint16_t pixel_x; // centroid in 96×96 input space
  uint16_t pixel_y;
  float confidence; // 0.0–1.0
} fomo_detection_t;

// Maximum detections returned per frame (12×12 grid = 144 cells max)
#define FOMO_MAX_DETECTIONS 32

// ── Result from a single detect call ─────────────────────────────────────────
typedef struct {
  fomo_detection_t detections[FOMO_MAX_DETECTIONS];
  int count;             // number of valid detections (0..FOMO_MAX_DETECTIONS)
  uint32_t inference_ms; // wall-clock time for capture + preprocess + invoke
} fomo_result_t;

// ── API ──────────────────────────────────────────────────────────────────────

/// Initialise the full pipeline: camera (OV2640) + TFLite Micro interpreter.
///
/// `model_data` / `model_data_len`: pointer to the .tflite flatbuffer
///     (from Rust's include_bytes!, must remain valid for program lifetime).
///
/// `arena_size_bytes`: how much heap to allocate for the TFLite tensor arena.
///     300 * 1024 is a good starting point for FOMO on ESP32.
///
/// Returns 0 on success, negative on error.
int fomo_init(const uint8_t *model_data, size_t model_data_len,
              size_t arena_size_bytes);

/// Capture a frame, run inference, return detections.
///
/// Writes results into `out`.  This function blocks for the duration of
/// capture + preprocessing + inference (~100-400 ms on ESP32 depending on
/// model size and whether ESP-NN kernels are active).
///
/// Returns 0 on success, negative on error.
int fomo_detect(fomo_result_t *out);

#ifdef __cplusplus
}
#endif
