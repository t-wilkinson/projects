// fomo.cpp — Camera + preprocess + TFLite Micro + output parsing

#include "fomo.h"

#include "esp_camera.h"
#include "esp_heap_caps.h"
#include "esp_log.h"
#include "esp_timer.h"
#include "img_converters.h"

#include "tensorflow/lite/micro/micro_interpreter.h"
#include "tensorflow/lite/micro/micro_log.h"
#include "tensorflow/lite/micro/micro_mutable_op_resolver.h"
#include "tensorflow/lite/schema/schema_generated.h"

#include <cmath>
#include <cstdlib>
#include <cstring>

static const char *TAG = "fomo";

// Scratch buffer for the decoded 160×120 RGB888 frame (57600 bytes)
static uint8_t *s_decode_buf = nullptr;
#define DECODE_W 160
#define DECODE_H 120
#define DECODE_SIZE (DECODE_W * DECODE_H * 3)

// Model constants
#define INPUT_W 96
#define INPUT_H 96
#define INPUT_CH 3
#define INPUT_SIZE (INPUT_W * INPUT_H * INPUT_CH)
#define NUM_CLASSES 6
#define CONF_THRESH 0.45f

// ImageNet normalisation (baked into training pipeline)
static const float IMAGENET_MEAN[3] = {0.485f, 0.456f, 0.406f};
static const float IMAGENET_STD[3] = {0.229f, 0.224f, 0.225f};

// ── AI-Thinker ESP32-CAM GPIO pin assignments ────────────────────────────────
#define CAM_PIN_PWDN 32
#define CAM_PIN_RESET (-1)
#define CAM_PIN_XCLK 0
#define CAM_PIN_SIOD 26
#define CAM_PIN_SIOC 27
#define CAM_PIN_D7 35
#define CAM_PIN_D6 34
#define CAM_PIN_D5 39
#define CAM_PIN_D4 36
#define CAM_PIN_D3 21
#define CAM_PIN_D2 19
#define CAM_PIN_D1 18
#define CAM_PIN_D0 5
#define CAM_PIN_VSYNC 25
#define CAM_PIN_HREF 23
#define CAM_PIN_PCLK 22

// ── Static state (lives for the whole program) ───────────────────────────────
static tflite::MicroInterpreter *s_interpreter = nullptr;
static tflite::MicroMutableOpResolver<20> s_resolver;
static uint8_t *s_arena = nullptr;

// Quantisation parameters read from the model's input tensor
static float s_input_scale = 0.0f;
static int32_t s_input_zero_point = 0;

// Scratch buffers (allocated once in fomo_init)
static uint8_t *s_rgb_buf = nullptr;  // INPUT_SIZE bytes: 96×96×3 RGB888
static int8_t *s_input_buf = nullptr; // INPUT_SIZE bytes: quantised int8

/// Capture JPEG, decode to RGB888, then nearest-neighbour downscale to 96×96
static int camera_capture_rgb96(uint8_t *out) {
  ESP_LOGI(TAG, "Capturing frame...");

  camera_fb_t *fb = esp_camera_fb_get();
  if (!fb) {
    ESP_LOGE(TAG, "Camera capture failed");
    return -1;
  }

  // JPEG → RGB888  (160×120 → 57 600 bytes)
  bool ok = fmt2rgb888(fb->buf, fb->len, PIXFORMAT_JPEG, s_decode_buf);
  esp_camera_fb_return(fb);

  if (!ok) {
    ESP_LOGE(TAG, "JPEG decode failed");
    return -2;
  }

  // Nearest-neighbour downscale 160×120 → 96×96
  for (int dy = 0; dy < INPUT_H; dy++) {
    int sy = (dy * DECODE_H) / INPUT_H;
    for (int dx = 0; dx < INPUT_W; dx++) {
      int sx = (dx * DECODE_W) / INPUT_W;
      int src_idx = (sy * DECODE_W + sx) * 3;
      int dst_idx = (dy * INPUT_W + dx) * 3;
      out[dst_idx + 0] = s_decode_buf[src_idx + 0];
      out[dst_idx + 1] = s_decode_buf[src_idx + 1];
      out[dst_idx + 2] = s_decode_buf[src_idx + 2];
    }
  }

  return 0;
}

// ─────────────────────────────────────────────────────────────────────────────
// Camera
// ─────────────────────────────────────────────────────────────────────────────

static int camera_init(void) {
  camera_config_t config = {};

  config.pin_pwdn = CAM_PIN_PWDN;
  config.pin_reset = CAM_PIN_RESET;
  config.pin_xclk = CAM_PIN_XCLK;
  config.pin_sccb_sda = CAM_PIN_SIOD;
  config.pin_sccb_scl = CAM_PIN_SIOC;
  config.pin_d7 = CAM_PIN_D7;
  config.pin_d6 = CAM_PIN_D6;
  config.pin_d5 = CAM_PIN_D5;
  config.pin_d4 = CAM_PIN_D4;
  config.pin_d3 = CAM_PIN_D3;
  config.pin_d2 = CAM_PIN_D2;
  config.pin_d1 = CAM_PIN_D1;
  config.pin_d0 = CAM_PIN_D0;
  config.pin_vsync = CAM_PIN_VSYNC;
  config.pin_href = CAM_PIN_HREF;
  config.pin_pclk = CAM_PIN_PCLK;

  config.xclk_freq_hz = 10000000;
  config.ledc_timer = LEDC_TIMER_0;
  config.ledc_channel = LEDC_CHANNEL_0;

  // config.pixel_format = PIXFORMAT_RGB565;
  // config.frame_size = FRAMESIZE_QQVGA; // 160x120 // FRAMESIZE_96X96; //
  // FRAMESIZE_QVGA; // 320×240
  //
  // config.jpeg_quality = 12;
  // config.fb_count = 2;
  // config.grab_mode = CAMERA_GRAB_WHEN_EMPTY;
  // config.fb_location = CAMERA_FB_IN_PSRAM; // CAMERA_FB_IN_PSRAM;

  config.pixel_format = PIXFORMAT_JPEG;
  config.frame_size = FRAMESIZE_QQVGA; // 160×120
  config.jpeg_quality = 8; // low compression → fast decode, good detail
  config.fb_count = 1;
  config.grab_mode = CAMERA_GRAB_WHEN_EMPTY;
  config.fb_location = CAMERA_FB_IN_PSRAM; // CAMERA_FB_IN_DRAM;

  ESP_LOGE(TAG, "initialising camera");
  esp_err_t err = esp_camera_init(&config);
  if (err != ESP_OK) {
    ESP_LOGE(TAG, "esp_camera_init failed: 0x%x", err);
    return -1;
  }

  camera_fb_t *warmup = esp_camera_fb_get();
  if (warmup) {
    ESP_LOGI(TAG, "Warm-up frame: %u bytes", (unsigned)warmup->len);
    esp_camera_fb_return(warmup);
  } else {
    ESP_LOGW(TAG, "Warm-up capture failed — DMA may be starved");
  }

  ESP_LOGI(TAG, "Camera initialised (QVGA RGB565)");
  return 0;
}

/// Capture a frame and downscale RGB565 320×240 → RGB888 96×96
// static int camera_capture_rgb96(uint8_t *out) {
//   camera_fb_t *fb = esp_camera_fb_get();
//   if (!fb) {
//     ESP_LOGE(TAG, "Camera capture failed");
//     return -1;
//   }
//
//   const int src_w = fb->width;
//   const int src_h = fb->height;
//   const uint8_t *src = fb->buf;
//
//   // Direct RGB565 → RGB888 conversion, no rescale needed
//   // for (int i = 0; i < INPUT_W * INPUT_H; i++) {
//   //   uint16_t lo = src[i * 2];
//   //   uint16_t hi = src[i * 2 + 1];
//   //   uint16_t pixel = (hi << 8) | lo;
//
//   //   uint8_t r = (uint8_t)((pixel >> 11) & 0x1F);
//   //   uint8_t g = (uint8_t)((pixel >> 5) & 0x3F);
//   //   uint8_t b = (uint8_t)(pixel & 0x1F);
//
//   //   out[i * 3 + 0] = (r << 3) | (r >> 2);
//   //   out[i * 3 + 1] = (g << 2) | (g >> 4);
//   //   out[i * 3 + 2] = (b << 3) | (b >> 2);
//   // }
//
//   // Nearest-neighbour downscale RGB565 → RGB888
//   for (int dy = 0; dy < INPUT_H; dy++) {
//     int sy = (dy * src_h) / INPUT_H;
//     for (int dx = 0; dx < INPUT_W; dx++) {
//       int sx = (dx * src_w) / INPUT_W;
//
//       int idx = (sy * src_w + sx) * 2;
//       uint16_t lo = src[idx];
//       uint16_t hi = src[idx + 1];
//       uint16_t pixel = (hi << 8) | lo;
//
//       uint8_t r = (uint8_t)((pixel >> 11) & 0x1F);
//       uint8_t g = (uint8_t)((pixel >> 5) & 0x3F);
//       uint8_t b = (uint8_t)(pixel & 0x1F);
//
//       int out_idx = (dy * INPUT_W + dx) * 3;
//       out[out_idx + 0] = (r << 3) | (r >> 2);
//       out[out_idx + 1] = (g << 2) | (g >> 4);
//       out[out_idx + 2] = (b << 3) | (b >> 2);
//     }
//   }
//
//   esp_camera_fb_return(fb);
//   return 0;
// }

// ─────────────────────────────────────────────────────────────────────────────
// Preprocessing: RGB888 uint8 → quantised int8 (NCHW)
// ─────────────────────────────────────────────────────────────────────────────

static void preprocess(const uint8_t *rgb, int8_t *out, float scale,
                       int32_t zero_point) {
  const int total = INPUT_W * INPUT_H;

  for (int i = 0; i < total; i++) {
    for (int ch = 0; ch < 3; ch++) {
      float pixel_f = rgb[i * 3 + ch] / 255.0f;
      float normalised = (pixel_f - IMAGENET_MEAN[ch]) / IMAGENET_STD[ch];
      int32_t quantised =
          (int32_t)roundf(normalised / scale + (float)zero_point);

      if (quantised < -128)
        quantised = -128;
      if (quantised > 127)
        quantised = 127;

      // NHWC layout — change to `out[ch * total + i]` if your model is NCHW
      out[i * 3 + ch] = (int8_t)quantised;
    }
  }
}

// ─────────────────────────────────────────────────────────────────────────────
// Output parsing: int8 grid → detections
// ─────────────────────────────────────────────────────────────────────────────

static int parse_output(const int8_t *data, const int *shape, int ndims,
                        fomo_detection_t *dets, int max_dets) {
  if (ndims != 4) {
    ESP_LOGE(TAG, "Expected 4-D output, got %d-D", ndims);
    return 0;
  }

  // Determine NCHW vs NHWC
  int is_nchw, c, h, w;
  if (shape[1] == NUM_CLASSES) {
    is_nchw = 1;
    c = shape[1];
    h = shape[2];
    w = shape[3];
  } else {
    is_nchw = 0;
    c = shape[3];
    h = shape[1];
    w = shape[2];
  }

  float cell_w = (float)INPUT_W / (float)w;
  float cell_h = (float)INPUT_H / (float)h;
  int count = 0;

  for (int gy = 0; gy < h && count < max_dets; gy++) {
    for (int gx = 0; gx < w && count < max_dets; gx++) {
      // Argmax over classes
      int best_cls = 0;
      int8_t best_val = -128;

      for (int cls = 0; cls < c; cls++) {
        int8_t val = is_nchw ? data[cls * h * w + gy * w + gx]
                             : data[gy * w * c + gx * c + cls];

        if (val > best_val) {
          best_val = val;
          best_cls = cls;
        }
      }

      // Skip background
      if (best_cls == 0)
        continue;

      // Sigmoid confidence from margin over background
      int8_t bg_val = is_nchw ? data[gy * w + gx] : data[gy * w * c + gx * c];

      float margin = (float)(best_val - bg_val) / 128.0f;
      float confidence = 1.0f / (1.0f + expf(-margin * 4.0f));

      if (confidence < CONF_THRESH)
        continue;

      fomo_detection_t *d = &dets[count++];
      d->class_id = (uint8_t)best_cls;
      d->grid_x = (uint8_t)gx;
      d->grid_y = (uint8_t)gy;
      d->_pad = 0;
      d->pixel_x = (uint16_t)(((float)gx + 0.5f) * cell_w);
      d->pixel_y = (uint16_t)(((float)gy + 0.5f) * cell_h);
      d->confidence = confidence;
    }
  }

  return count;
}

// ─────────────────────────────────────────────────────────────────────────────
// Public API
// ─────────────────────────────────────────────────────────────────────────────

extern "C" int fomo_init(const uint8_t *model_data, size_t model_data_len,
                         size_t arena_size_bytes) {
  // ── Camera ───────────────────────────────────────────────────────────
  if (camera_init() != 0) {
    return -1;
  }

  // ── Allocate scratch buffers ─────────────────────────────────────────
  s_rgb_buf = (uint8_t *)malloc(INPUT_SIZE);
  s_input_buf = (int8_t *)malloc(INPUT_SIZE);
  if (!s_rgb_buf || !s_input_buf) {
    ESP_LOGE(TAG, "Failed to allocate scratch buffers (%d bytes each)",
             INPUT_SIZE);
    return -2;
  }

  s_decode_buf = (uint8_t *)malloc(DECODE_SIZE);
  if (!s_decode_buf) {
    ESP_LOGE(TAG, "Failed to allocate JPEG decode buffer (%d bytes)",
             DECODE_SIZE);
    return -2;
  }
  ESP_LOGI(TAG, "Allocated scratch buffers");

  // ── Parse model ──────────────────────────────────────────────────────
  const tflite::Model *model = tflite::GetModel(model_data);
  if (!model || model->version() != TFLITE_SCHEMA_VERSION) {
    ESP_LOGE(TAG, "Invalid model (version=%lu, expected=%d)",
             model ? (unsigned long)model->version() : 0,
             TFLITE_SCHEMA_VERSION);
    return -3;
  }

  ESP_LOGI(TAG, "Parsed model");

  // ── Allocate arena & build interpreter ────────────────────────────────
  s_arena = (uint8_t *)malloc(arena_size_bytes);
  if (!s_arena) {
    ESP_LOGE(TAG, "Failed to allocate tensor arena (%u bytes)",
             (unsigned)arena_size_bytes);
    return -4;
  }

  ESP_LOGI(TAG, "Allocated arena");

  s_resolver.AddConv2D();
  s_resolver.AddDepthwiseConv2D();
  s_resolver.AddReshape();
  s_resolver.AddSoftmax();
  s_resolver.AddPad();
  s_resolver.AddPadV2();
  s_resolver.AddAdd();
  s_resolver.AddMul();
  s_resolver.AddMean();
  s_resolver.AddRelu6();
  s_resolver.AddQuantize();
  s_resolver.AddDequantize();
  s_resolver.AddFullyConnected();
  s_resolver.AddLogistic();
  s_resolver.AddExpandDims();
  s_resolver.AddConcatenation();
  s_resolver.AddStridedSlice();
  s_interpreter = new (std::nothrow)
      tflite::MicroInterpreter(model, s_resolver, s_arena, arena_size_bytes);

  if (!s_interpreter) {
    ESP_LOGE(TAG, "Failed to create MicroInterpreter");
    return -5;
  }

  ESP_LOGI(TAG, "Allocated interpreter");

  TfLiteStatus status = s_interpreter->AllocateTensors();
  if (status != kTfLiteOk) {
    ESP_LOGE(TAG, "AllocateTensors() failed (status=%d)", status);
    return -6;
  }

  ESP_LOGI(TAG, "Allocated tensors");

  // ── Read quantisation parameters ─────────────────────────────────────
  TfLiteTensor *input_tensor = s_interpreter->input(0);
  if (input_tensor && input_tensor->quantization.params) {
    auto *qp = static_cast<TfLiteAffineQuantization *>(
        input_tensor->quantization.params);
    if (qp->scale && qp->scale->size > 0)
      s_input_scale = qp->scale->data[0];
    if (qp->zero_point && qp->zero_point->size > 0)
      s_input_zero_point = qp->zero_point->data[0];
  }

  ESP_LOGI(TAG, "Read quantisation parameters");

  // Fallback for common uint8→int8 mapping
  if (s_input_scale == 0.0f) {
    s_input_scale = 0.003921568859368563f; // 1/255
    s_input_zero_point = -128;
  }

  ESP_LOGI(TAG, "Model loaded: arena %u/%u bytes, input quant scale=%.6f zp=%d",
           (unsigned)s_interpreter->arena_used_bytes(),
           (unsigned)arena_size_bytes, s_input_scale, (int)s_input_zero_point);

  // // ── Override OV2640 clock for raw RGB565 on ESP32 ────────────────
  // // The driver forces clk_2x=0 on ESP32, which starves the DSP of
  // // clock cycles and prevents HREF from asserting in non-JPEG modes.
  // sensor_t *s = esp_camera_sensor_get();
  // if (s) {
  //   // CLKRC (bank 1, reg 0x11): set bit 7 = clk_2x ON, bits 5:0 = clk_div 3
  //   //   → internal DSP clock = XCLK × 2 / (3+1) = 10 MHz
  //   s->set_reg(s, 0x111, 0xFF, 0x83); // 0x80 | 3

  //   // R_DVP_SP (bank 0, reg 0xD3): clear bit 7 = pclk_auto OFF, bits 6:0 =
  //   // pclk_div 8
  //   //   → deterministic PCLK the I2S DMA can lock onto
  //   s->set_reg(s, 0x0D3, 0xFF, 0x08);
  // }
  // ESP_LOGI(TAG, "Updated camera registers");

  ESP_LOGI(TAG, "Free internal: %u, largest DMA block: %u",
           (unsigned)heap_caps_get_free_size(MALLOC_CAP_INTERNAL),
           (unsigned)heap_caps_get_largest_free_block(MALLOC_CAP_DMA));

  return 0;
}

extern "C" int fomo_detect(fomo_result_t *out) {
  if (!s_interpreter || !out)
    return -1;

  int64_t t_start = esp_timer_get_time();

  // 1. Capture + downscale → 96×96 RGB888
  if (camera_capture_rgb96(s_rgb_buf) != 0) {
    return -2;
  }

  // 2. Preprocess → quantised int8
  preprocess(s_rgb_buf, s_input_buf, s_input_scale, s_input_zero_point);

  // 3. Copy into input tensor
  TfLiteTensor *input_tensor = s_interpreter->input(0);
  memcpy(input_tensor->data.raw, s_input_buf, INPUT_SIZE);

  // 4. Invoke
  TfLiteStatus status = s_interpreter->Invoke();
  if (status != kTfLiteOk) {
    ESP_LOGE(TAG, "Invoke failed (status=%d)", status);
    return -3;
  }

  // 5. Parse output
  const TfLiteTensor *output_tensor = s_interpreter->output(0);
  int ndims = output_tensor->dims->size;
  int shape[4] = {0};
  for (int i = 0; i < ndims && i < 4; i++) {
    shape[i] = output_tensor->dims->data[i];
  }

  out->count = parse_output((const int8_t *)output_tensor->data.raw, shape,
                            ndims, out->detections, FOMO_MAX_DETECTIONS);

  int64_t elapsed_us = esp_timer_get_time() - t_start;
  out->inference_ms = (uint32_t)(elapsed_us / 1000);

  return 0;
}
