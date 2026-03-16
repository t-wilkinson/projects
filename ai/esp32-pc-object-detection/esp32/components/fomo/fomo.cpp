// fomo.cpp — Camera + preprocess + TFLite Micro + output parsing

#include "fomo.h"

#include "esp_camera.h"
#include "esp_heap_caps.h"
#include "esp_log.h"
#include "esp_timer.h"
#include "img_converters.h"

#include "freertos/FreeRTOS.h"
#include "freertos/task.h"

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

#define BMP_HEADER_SIZE 54
#define BMP_DATA_SIZE (INPUT_W * INPUT_H * 3)
#define BMP_TOTAL_SIZE (BMP_HEADER_SIZE + BMP_DATA_SIZE)

static uint8_t s_bmp_buf[BMP_TOTAL_SIZE];

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
static float s_output_scale = 0.0f;
static int32_t s_output_zero_point = 0;
static int64_t t_log;

// Scratch buffers (allocated once in fomo_init)
static uint8_t *s_rgb_buf = nullptr;  // INPUT_SIZE bytes: 96×96×3 RGB888
static int8_t *s_input_buf = nullptr; // INPUT_SIZE bytes: quantised int8

static int camera_capture_rgb96(uint8_t *out) {
  camera_fb_t *fb = esp_camera_fb_get();
  if (!fb) {
    ESP_LOGE(TAG, "Camera capture failed");
    return -1;
  }

  // DEBUG: raw frame buffer info + first bytes
  ESP_LOGI(TAG, "fb: %p len=%d w=%d h=%d fmt=%d", fb->buf, fb->len, fb->width,
           fb->height, fb->format);
  ESP_LOGI(TAG, "Raw hex: %02x %02x %02x %02x  %02x %02x %02x %02x  %02x %02x",
           fb->buf[0], fb->buf[1], fb->buf[2], fb->buf[3], fb->buf[4],
           fb->buf[5], fb->buf[6], fb->buf[7], fb->buf[8], fb->buf[9]);

  // DEBUG: check if adjacent pixels are similar (they should be in a real
  // image) Pixel 0 vs Pixel 1 (bytes 0-1 vs bytes 2-3)
  uint16_t px0 = (fb->buf[0] << 8) | fb->buf[1];
  uint16_t px1 = (fb->buf[2] << 8) | fb->buf[3];
  uint16_t px0_swap = (fb->buf[1] << 8) | fb->buf[0];
  uint16_t px1_swap = (fb->buf[3] << 8) | fb->buf[2];
  ESP_LOGI(TAG, "BigEnd px0=0x%04x px1=0x%04x  LitEnd px0=0x%04x px1=0x%04x",
           px0, px1, px0_swap, px1_swap);

  const uint8_t *src = fb->buf;
  for (int i = 0; i < INPUT_W * INPUT_H; i++) {
    uint16_t pixel = (src[i * 2] << 8) | src[i * 2 + 1]; // swapped

    uint8_t r = (uint8_t)((pixel >> 11) & 0x1F);
    uint8_t g = (uint8_t)((pixel >> 5) & 0x3F);
    uint8_t b = (uint8_t)(pixel & 0x1F);

    out[i * 3 + 0] = (r << 3) | (r >> 2);
    out[i * 3 + 1] = (g << 2) | (g >> 4);
    out[i * 3 + 2] = (b << 3) | (b >> 2);
  }

  esp_camera_fb_return(fb);
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

  config.pixel_format = PIXFORMAT_RGB565;
  config.frame_size = FRAMESIZE_96X96;
  config.fb_count = 2;
  // config.fb_location = CAMERA_FB_IN_DRAM;
  config.fb_location = CAMERA_FB_IN_PSRAM;
  config.grab_mode = CAMERA_GRAB_LATEST;

  ESP_LOGE(TAG, "initialising camera");
  esp_err_t err = esp_camera_init(&config);
  if (err != ESP_OK) {
    ESP_LOGE(TAG, "esp_camera_init failed: 0x%x", err);
    return -1;
  }

  ESP_LOGI(TAG, "Camera initialised (QVGA RGB565)");
  return 0;
}

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

      // Dequantize all classes and compute softmax
      float logits[NUM_CLASSES];
      float max_logit = -1e9f;

      for (int cls = 0; cls < c; cls++) {
        int8_t raw = is_nchw ? data[cls * h * w + gy * w + gx]
                             : data[gy * w * c + gx * c + cls];
        logits[cls] =
            ((float)raw - (float)s_output_zero_point) * s_output_scale;
        if (logits[cls] > max_logit)
          max_logit = logits[cls];
      }

      // Softmax + argmax (skip background for best-class search)
      float sum_exp = 0.0f;
      for (int cls = 0; cls < c; cls++) {
        logits[cls] = expf(logits[cls] - max_logit); // stable exp
        sum_exp += logits[cls];
      }

      int best_cls = 0;
      float best_prob = 0.0f;
      for (int cls = 1; cls < c; cls++) { // skip background (cls 0)
        float prob = logits[cls] / sum_exp;
        if (prob > best_prob) {
          best_prob = prob;
          best_cls = cls;
        }
      }

      if (best_cls == 0 || best_prob < CONF_THRESH)
        continue;

      fomo_detection_t *d = &dets[count++];
      d->class_id = (uint8_t)best_cls;
      d->grid_x = (uint8_t)gx;
      d->grid_y = (uint8_t)gy;
      d->_pad = 0;
      d->pixel_x = (uint16_t)(((float)gx + 0.5f) * cell_w);
      d->pixel_y = (uint16_t)(((float)gy + 0.5f) * cell_h);
      d->confidence = best_prob;
    }
  }
  return count;
}

void log_time(const char *msg) {
  int64_t t_now = esp_timer_get_time();
  int64_t elapsed_us = t_now - t_log;
  uint32_t t_diff = (uint32_t)(elapsed_us / 1000);
  ESP_LOGI(TAG, "time: %s took %lu ms", msg, t_diff);
  t_log = t_now;
}

/// Call AFTER camera_capture_rgb96 has filled s_rgb_buf
extern "C" const uint8_t *fomo_get_bmp(int *out_len) {
  // BMP header (54 bytes)
  uint8_t *h = s_bmp_buf;
  memset(h, 0, BMP_HEADER_SIZE);

  int w = INPUT_W, ht = INPUT_H;
  int file_size = BMP_TOTAL_SIZE;
  int data_size = BMP_DATA_SIZE;

  // Signature
  h[0] = 'B';
  h[1] = 'M';
  // File size
  h[2] = file_size;
  h[3] = file_size >> 8;
  h[4] = file_size >> 16;
  h[5] = file_size >> 24;
  // Data offset
  h[10] = BMP_HEADER_SIZE;
  // DIB header size (BITMAPINFOHEADER = 40)
  h[14] = 40;
  // Width
  h[18] = w;
  h[19] = w >> 8;
  // Height (negative = top-down, so we don't have to flip rows)
  int neg_h = -ht;
  h[22] = neg_h;
  h[23] = neg_h >> 8;
  h[24] = neg_h >> 16;
  h[25] = neg_h >> 24;
  // Planes
  h[26] = 1;
  // Bits per pixel
  h[28] = 24;
  // Image data size
  h[34] = data_size;
  h[35] = data_size >> 8;
  h[36] = data_size >> 16;
  h[37] = data_size >> 24;

  // Copy RGB888 → BGR888 (BMP pixel order)
  uint8_t *dst = s_bmp_buf + BMP_HEADER_SIZE;
  for (int i = 0; i < w * ht; i++) {
    dst[i * 3 + 0] = s_rgb_buf[i * 3 + 2]; // B
    dst[i * 3 + 1] = s_rgb_buf[i * 3 + 1]; // G
    dst[i * 3 + 2] = s_rgb_buf[i * 3 + 0]; // R
  }

  *out_len = BMP_TOTAL_SIZE;
  return s_bmp_buf;
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
  // s_rgb_buf = (uint8_t *)malloc(INPUT_SIZE);
  // s_input_buf = (int8_t *)malloc(INPUT_SIZE);
  s_rgb_buf = (uint8_t *)heap_caps_malloc(INPUT_SIZE, MALLOC_CAP_SPIRAM);
  s_input_buf = (int8_t *)heap_caps_malloc(INPUT_SIZE, MALLOC_CAP_SPIRAM);
  if (!s_rgb_buf || !s_input_buf) {
    ESP_LOGE(TAG, "Failed to allocate scratch buffers (%d bytes each)",
             INPUT_SIZE);
    return -2;
  }

  // s_decode_buf = (uint8_t *)malloc(DECODE_SIZE);
  // s_decode_buf = (uint8_t *)heap_caps_malloc(DECODE_SIZE, MALLOC_CAP_INTERNAL
  // |
  //                                                             MALLOC_CAP_8BIT);
  // if (!s_decode_buf) {
  //   ESP_LOGE(TAG, "Failed to allocate JPEG decode buffer (%d bytes)",
  //            DECODE_SIZE);
  //   return -2;
  // }
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
  s_arena = (uint8_t *)heap_caps_malloc(arena_size_bytes, MALLOC_CAP_SPIRAM);
  // s_arena = (uint8_t *)malloc(arena_size_bytes);
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

  TfLiteTensor *output_tensor = s_interpreter->output(0);
  if (output_tensor && output_tensor->quantization.params) {
    auto *qp = static_cast<TfLiteAffineQuantization *>(
        output_tensor->quantization.params);
    if (qp->scale && qp->scale->size > 0)
      s_output_scale = qp->scale->data[0];
    if (qp->zero_point && qp->zero_point->size > 0)
      s_output_zero_point = qp->zero_point->data[0];
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

  return 0;
}
static bool tested = false;

extern "C" int fomo_detect(fomo_result_t *out) {
  if (!s_interpreter || !out)
    return -1;

  int64_t t_start = esp_timer_get_time();
  t_log = t_start;

  // 1. Capture + downscale → 96×96 RGB888
  if (camera_capture_rgb96(s_rgb_buf) != 0) {
    return -2;
  }
  log_time("camera_capture_rgb96");

  // DEBUG: check raw RGB888 pixel values
  ESP_LOGI(TAG, "RGB sample: [0]=%d,%d,%d [100]=%d,%d,%d [4600]=%d,%d,%d",
           s_rgb_buf[0], s_rgb_buf[1], s_rgb_buf[2], s_rgb_buf[300],
           s_rgb_buf[301], s_rgb_buf[302], s_rgb_buf[13800], s_rgb_buf[13801],
           s_rgb_buf[13802]);

  // 2. Preprocess → quantised int8
  preprocess(s_rgb_buf, s_input_buf, s_input_scale, s_input_zero_point);

  log_time("preprocess");

  // DEBUG: check quantised input values
  ESP_LOGI(TAG, "Input sample: [0]=%d,%d,%d [100]=%d,%d,%d", s_input_buf[0],
           s_input_buf[1], s_input_buf[2], s_input_buf[300], s_input_buf[301],
           s_input_buf[302]);

  // 3. Copy into input tensor
  TfLiteTensor *input_tensor = s_interpreter->input(0);
  memcpy(input_tensor->data.raw, s_input_buf, INPUT_SIZE);
  log_time("copy into input tensor");

  // DEBUG: dump raw output tensor for cell (0,0) and (6,6)
  const TfLiteTensor *output_tensor = s_interpreter->output(0);
  const int8_t *odata = (const int8_t *)output_tensor->data.raw;
  // NHWC: index = (gy * 12 + gx) * 6 + cls
  ESP_LOGI(TAG, "Output cell(0,0): %d %d %d %d %d %d", odata[0], odata[1],
           odata[2], odata[3], odata[4], odata[5]);
  ESP_LOGI(TAG, "Output cell(6,6): %d %d %d %d %d %d",
           odata[(6 * 12 + 6) * 6 + 0], odata[(6 * 12 + 6) * 6 + 1],
           odata[(6 * 12 + 6) * 6 + 2], odata[(6 * 12 + 6) * 6 + 3],
           odata[(6 * 12 + 6) * 6 + 4], odata[(6 * 12 + 6) * 6 + 5]);

  // 4. Invoke
  TfLiteStatus status = s_interpreter->Invoke();
  if (status != kTfLiteOk) {
    ESP_LOGE(TAG, "Invoke failed (status=%d)", status);
    return -3;
  }
  log_time("tflite invoke()");

  if (!tested) {
    tested = true;

    // Fill with uniform mid-grey RGB888 (128,128,128)
    uint8_t *test_rgb = s_rgb_buf;
    memset(test_rgb, 128, INPUT_SIZE);

    preprocess(test_rgb, s_input_buf, s_input_scale, s_input_zero_point);
    memcpy(input_tensor->data.raw, s_input_buf, INPUT_SIZE);

    s_interpreter->Invoke();

    const int8_t *tdata = (const int8_t *)output_tensor->data.raw;
    // NHWC: dump a few cells
    ESP_LOGI(TAG, "GREY TEST cell(0,0): %d %d %d %d %d %d", tdata[0], tdata[1],
             tdata[2], tdata[3], tdata[4], tdata[5]);
    ESP_LOGI(TAG, "GREY TEST cell(6,6): %d %d %d %d %d %d",
             tdata[(6 * 12 + 6) * 6 + 0], tdata[(6 * 12 + 6) * 6 + 1],
             tdata[(6 * 12 + 6) * 6 + 2], tdata[(6 * 12 + 6) * 6 + 3],
             tdata[(6 * 12 + 6) * 6 + 4], tdata[(6 * 12 + 6) * 6 + 5]);

    // Count how many cells have bg as argmax
    int bg_wins = 0;
    for (int i = 0; i < 144; i++) {
      int best = 0;
      int8_t best_val = tdata[i * 6];
      for (int c = 1; c < 6; c++) {
        if (tdata[i * 6 + c] > best_val) {
          best_val = tdata[i * 6 + c];
          best = c;
        }
      }
      if (best == 0)
        bg_wins++;
    }
    ESP_LOGI(TAG, "GREY TEST: bg wins %d/144 cells", bg_wins);
  }

  // 5. Parse output
  // const TfLiteTensor *output_tensor = s_interpreter->output(0);
  int ndims = output_tensor->dims->size;
  int shape[4] = {0};
  for (int i = 0; i < ndims && i < 4; i++) {
    shape[i] = output_tensor->dims->data[i];
  }

  out->count = parse_output((const int8_t *)output_tensor->data.raw, shape,
                            ndims, out->detections, FOMO_MAX_DETECTIONS);
  log_time("parse output");

  int64_t elapsed_us = esp_timer_get_time() - t_start;
  out->inference_ms = (uint32_t)(elapsed_us / 1000);

  return 0;
}
