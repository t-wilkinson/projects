// ─────────────────────────────────────────────────────────────────────────────
// fomo.cpp — camera capture → preprocess → TFLite Micro inference
//
// Nothing in here hard-codes the model's input size, channel count or class
// count: they are read from the .tflite tensors in fomo_init(), so a model
// re-exported from the notebook at a new resolution drops straight in.
// ─────────────────────────────────────────────────────────────────────────────

#include "fomo.h"

#include "esp_camera.h"
#include "esp_heap_caps.h"
#include "esp_log.h"
#include "esp_timer.h"
#include "sdkconfig.h"

#include "tensorflow/lite/micro/micro_interpreter.h"
#include "tensorflow/lite/micro/micro_mutable_op_resolver.h"
#include "tensorflow/lite/schema/schema_generated.h"

#include <cmath>
#include <cstdlib>
#include <cstring>
#include <new>

static const char *TAG = "fomo";

// ── Error codes returned to Rust ─────────────────────────────────────────────
enum {
  FOMO_ERR_ARGS = -1,
  FOMO_ERR_CAMERA_INIT = -2,
  FOMO_ERR_MODEL_VERSION = -3,
  FOMO_ERR_ARENA_ALLOC = -4,
  FOMO_ERR_ALLOCATE_TENSORS = -5,
  FOMO_ERR_TENSOR_LAYOUT = -6,
  FOMO_ERR_CAPTURE = -7,
  FOMO_ERR_FRAME_FORMAT = -8,
  FOMO_ERR_INVOKE = -9,
};

// ── Camera pins ──────────────────────────────────────────────────────────────
// Note for later: the camera's XCLK uses LEDC timer 0 / channel 0. Use other
// LEDC timers/channels for motor PWM.
#if CONFIG_IDF_TARGET_ESP32S3
// Freenove ESP32-S3 WROOM CAM (same wiring as Espressif's ESP32-S3-EYE)
#define CAM_PIN_PWDN (-1)
#define CAM_PIN_RESET (-1)
#define CAM_PIN_XCLK 15
#define CAM_PIN_SIOD 4
#define CAM_PIN_SIOC 5
#define CAM_PIN_D7 16
#define CAM_PIN_D6 17
#define CAM_PIN_D5 18
#define CAM_PIN_D4 12
#define CAM_PIN_D3 10
#define CAM_PIN_D2 8
#define CAM_PIN_D1 9
#define CAM_PIN_D0 11
#define CAM_PIN_VSYNC 6
#define CAM_PIN_HREF 7
#define CAM_PIN_PCLK 13
#define CAM_XCLK_HZ 20000000
#else
// AI-Thinker ESP32-CAM
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
#define CAM_XCLK_HZ 10000000
#endif

// ── State that lives for the whole program ──────────────────────────────────
// Every op any of the notebook's exports has used, plus the usual suspects.
// Unused ops only cost flash, which the S3 has plenty of.
static tflite::MicroMutableOpResolver<20> s_resolver;
static const tflite::Model *s_model = nullptr;
static tflite::MicroInterpreter *s_interpreter = nullptr;
static uint8_t *s_arena = nullptr;

static fomo_info_t s_info = {};
static uint8_t *s_image = nullptr; // pixels the model saw (for the web view)

// Quantisation lookup table: input byte value → int8 tensor value, per
// channel. A pixel can only be 0–255, so the float maths
//   round(((v/255 - mean) / std) / scale + zero_point)
// is done 256 × channels times at start-up instead of once per pixel.
static int8_t s_lut[3][256];

// Centred crop of the camera frame with the model input's aspect ratio.
static int s_crop_x0, s_crop_y0, s_crop_w, s_crop_h;

static inline uint32_t elapsed_us(int64_t since) {
  return (uint32_t)(esp_timer_get_time() - since);
}

static void log_heap(const char *stage) {
  ESP_LOGI(TAG,
           "[mem] %-22s internal free %6u B (largest block %6u)  "
           "psram free %7u B",
           stage, (unsigned)heap_caps_get_free_size(MALLOC_CAP_INTERNAL),
           (unsigned)heap_caps_get_largest_free_block(MALLOC_CAP_INTERNAL |
                                                      MALLOC_CAP_8BIT),
           (unsigned)heap_caps_get_free_size(MALLOC_CAP_SPIRAM));
}

// ─────────────────────────────────────────────────────────────────────────────
// Camera
// ─────────────────────────────────────────────────────────────────────────────

// Smallest sensor frame that covers the model input in both directions, so
// the centre crop needs as little resampling as possible. A 120×120 or 160×96
// model on a 160×120 frame is a pure 1:1 crop.
static framesize_t pick_frame_size(int w, int h) {
  struct {
    framesize_t size;
    int w, h;
  } sizes[] = { // ordered by area
      {FRAMESIZE_96X96, 96, 96},    {FRAMESIZE_128X128, 128, 128},
      {FRAMESIZE_QQVGA, 160, 120},  {FRAMESIZE_QCIF, 176, 144},
      {FRAMESIZE_HQVGA, 240, 176},  {FRAMESIZE_240X240, 240, 240},
      {FRAMESIZE_QVGA, 320, 240},   {FRAMESIZE_VGA, 640, 480},
  };
  for (auto &s : sizes) {
    if (s.w >= w && s.h >= h)
      return s.size;
  }
  return FRAMESIZE_VGA;
}

static int camera_init(int w, int h, bool vflip, bool hmirror) {
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
  config.xclk_freq_hz = CAM_XCLK_HZ;
  config.ledc_timer = LEDC_TIMER_0;
  config.ledc_channel = LEDC_CHANNEL_0;

  // RGB565 straight from the sensor: no JPEG decode on the CPU.
  config.pixel_format = PIXFORMAT_RGB565;
  config.frame_size = pick_frame_size(w, h);
  // Two frame buffers in PSRAM: the camera DMA fills one while we read the
  // other. GRAB_LATEST hands us the newest frame, not a stale queued one.
  config.fb_count = 2;
  config.fb_location = CAMERA_FB_IN_PSRAM;
  config.grab_mode = CAMERA_GRAB_LATEST;

  esp_err_t err = esp_camera_init(&config);
  if (err != ESP_OK) {
    ESP_LOGE(TAG, "esp_camera_init failed: 0x%x", err);
    return FOMO_ERR_CAMERA_INIT;
  }

  sensor_t *s = esp_camera_sensor_get();
  s_info.camera_pid = s->id.PID;
  s->set_vflip(s, vflip);
  s->set_hmirror(s, hmirror);

  // The sensor's auto exposure/white balance needs a few frames to settle,
  // and the first frame tells us the real frame size.
  camera_fb_t *fb = nullptr;
  for (int i = 0; i < 3; i++) {
    if (fb)
      esp_camera_fb_return(fb);
    fb = esp_camera_fb_get();
  }
  if (!fb) {
    ESP_LOGE(TAG, "camera gave no frame");
    return FOMO_ERR_CAPTURE;
  }
  s_info.camera_w = fb->width;
  s_info.camera_h = fb->height;
  esp_camera_fb_return(fb);

  ESP_LOGI(TAG, "camera PID=0x%04x, frames %dx%d RGB565", s_info.camera_pid,
           (int)s_info.camera_w, (int)s_info.camera_h);
  return 0;
}

extern "C" void fomo_set_orientation(bool vflip, bool hmirror) {
  sensor_t *s = esp_camera_sensor_get();
  if (!s)
    return;
  s->set_vflip(s, vflip);
  s->set_hmirror(s, hmirror);
}

// ─────────────────────────────────────────────────────────────────────────────
// Interpreter + tensor arena
// ─────────────────────────────────────────────────────────────────────────────

static void register_ops(void) {
  s_resolver.AddConv2D();
  s_resolver.AddDepthwiseConv2D();
  s_resolver.AddAdd();
  s_resolver.AddMul();
  s_resolver.AddPad();
  s_resolver.AddPadV2();
  s_resolver.AddReshape();
  s_resolver.AddTranspose();
  s_resolver.AddConcatenation();
  s_resolver.AddStridedSlice();
  s_resolver.AddMean();
  s_resolver.AddMaxPool2D();
  s_resolver.AddAveragePool2D();
  s_resolver.AddRelu();
  s_resolver.AddRelu6();
  s_resolver.AddHardSwish();
  s_resolver.AddLogistic();
  s_resolver.AddSoftmax();
  s_resolver.AddQuantize();
  s_resolver.AddDequantize();
}

static bool build_interpreter(uint8_t *arena, size_t size) {
  delete s_interpreter;
  s_interpreter = new (std::nothrow)
      tflite::MicroInterpreter(s_model, s_resolver, arena, size);
  if (!s_interpreter) {
    ESP_LOGE(TAG, "out of memory creating MicroInterpreter");
    return false;
  }
  if (s_interpreter->AllocateTensors() != kTfLiteOk) {
    ESP_LOGE(TAG, "AllocateTensors() failed with a %u byte arena",
             (unsigned)size);
    delete s_interpreter;
    s_interpreter = nullptr;
    return false;
  }
  return true;
}

// "Measure, then place", step 1: build the interpreter in a generous PSRAM
// arena and ask TFLM how much it actually used (activations + kernel
// scratch, planned so tensors that are never alive at the same time share
// memory).
static int probe_arena(size_t probe_bytes) {
  uint8_t *probe = (uint8_t *)heap_caps_aligned_alloc(
      16, probe_bytes, MALLOC_CAP_SPIRAM | MALLOC_CAP_8BIT);
  bool in_psram = probe != nullptr;
  if (!probe) // no PSRAM: try internal SRAM directly
    probe = (uint8_t *)heap_caps_aligned_alloc(
        16, probe_bytes, MALLOC_CAP_INTERNAL | MALLOC_CAP_8BIT);
  if (!probe) {
    ESP_LOGE(TAG, "can't allocate a %u byte probe arena", (unsigned)probe_bytes);
    return FOMO_ERR_ARENA_ALLOC;
  }
  if (!build_interpreter(probe, probe_bytes)) {
    heap_caps_free(probe);
    return FOMO_ERR_ALLOCATE_TENSORS;
  }

  s_arena = probe;
  s_info.arena_bytes = probe_bytes;
  s_info.arena_in_psram = in_psram;
  s_info.arena_used_bytes = s_interpreter->arena_used_bytes();
  ESP_LOGI(TAG, "model needs %u B of tensor arena",
           (unsigned)s_info.arena_used_bytes);
  return 0;
}

// Step 2, once everything else that needs internal SRAM (camera DMA
// descriptors, WiFi) has allocated: if the arena fits in internal SRAM with
// room to spare, rebuild the interpreter there. Internal SRAM is
// single-cycle; PSRAM goes through the same cache as the model weights in
// flash, so an arena in PSRAM means cache misses on every layer's
// activations.
static int place_arena(size_t internal_reserve) {
  if (!s_info.arena_in_psram)
    return 0;

  // Small slack for alignment padding.
  size_t need = ((s_info.arena_used_bytes + 15) & ~(size_t)15) + 1024;
  size_t internal_free = heap_caps_get_free_size(MALLOC_CAP_INTERNAL);
  size_t internal_block = heap_caps_get_largest_free_block(
      MALLOC_CAP_INTERNAL | MALLOC_CAP_8BIT);

  if (internal_block < need || internal_free < need + internal_reserve) {
    ESP_LOGW(TAG,
             "arena stays in PSRAM: needs %u B, internal SRAM has %u B free "
             "(largest block %u B) and %u B must stay free",
             (unsigned)need, (unsigned)internal_free, (unsigned)internal_block,
             (unsigned)internal_reserve);
    return 0;
  }

  uint8_t *internal = (uint8_t *)heap_caps_aligned_alloc(
      16, need, MALLOC_CAP_INTERNAL | MALLOC_CAP_8BIT);
  if (internal && build_interpreter(internal, need)) {
    heap_caps_free(s_arena);
    s_arena = internal;
    s_info.arena_bytes = need;
    s_info.arena_in_psram = false;
    ESP_LOGI(TAG, "arena moved to internal SRAM (%u B)", (unsigned)need);
    return 0;
  }

  // Fall back to the PSRAM arena we know works.
  ESP_LOGW(TAG, "couldn't move arena to internal SRAM; staying in PSRAM");
  if (internal)
    heap_caps_free(internal);
  return build_interpreter(s_arena, s_info.arena_bytes)
             ? 0
             : FOMO_ERR_ALLOCATE_TENSORS;
}

static float tensor_scale(const TfLiteTensor *t) {
  return t->params.scale;
}

static int read_tensor_layout(void) {
  TfLiteTensor *in = s_interpreter->input(0);
  TfLiteTensor *out = s_interpreter->output(0);

  if (in->type != kTfLiteInt8 || out->type != kTfLiteInt8) {
    ESP_LOGE(TAG, "expected int8 input and output (got types %d / %d); "
                  "export with inference_input_type/output_type = tf.int8",
             in->type, out->type);
    return FOMO_ERR_TENSOR_LAYOUT;
  }
  if (in->dims->size != 4 || out->dims->size != 4) {
    ESP_LOGE(TAG, "expected 4-D NHWC tensors (got %d-D / %d-D)", in->dims->size,
             out->dims->size);
    return FOMO_ERR_TENSOR_LAYOUT;
  }

  s_info.input_h = in->dims->data[1];
  s_info.input_w = in->dims->data[2];
  s_info.input_c = in->dims->data[3];
  s_info.input_scale = tensor_scale(in);
  s_info.input_zero_point = in->params.zero_point;

  s_info.grid_h = out->dims->data[1];
  s_info.grid_w = out->dims->data[2];
  s_info.output_c = out->dims->data[3];
  s_info.output_scale = tensor_scale(out);
  s_info.output_zero_point = out->params.zero_point;

  if (s_info.input_c != 1 && s_info.input_c != 3) {
    ESP_LOGE(TAG, "input has %d channels; only 1 (gray) or 3 (RGB) supported. "
                  "Is the model NCHW? onnx2tf should produce NHWC.",
             (int)s_info.input_c);
    return FOMO_ERR_TENSOR_LAYOUT;
  }

  ESP_LOGI(TAG, "input  %dx%dx%d int8 scale=%.6f zp=%d", (int)s_info.input_w,
           (int)s_info.input_h, (int)s_info.input_c, s_info.input_scale,
           (int)s_info.input_zero_point);
  ESP_LOGI(TAG, "output %dx%dx%d int8 scale=%.6f zp=%d", (int)s_info.grid_w,
           (int)s_info.grid_h, (int)s_info.output_c, s_info.output_scale,
           (int)s_info.output_zero_point);
  return 0;
}

static void build_lut(const fomo_config_t *cfg) {
  for (int c = 0; c < s_info.input_c; c++) {
    float mean = cfg->norm_mean[c];
    float std = cfg->norm_std[c];
    for (int v = 0; v < 256; v++) {
      float x = ((float)v / 255.0f - mean) / std;
      int32_t q = (int32_t)lroundf(x / s_info.input_scale) + s_info.input_zero_point;
      s_lut[c][v] = (int8_t)(q < -128 ? -128 : (q > 127 ? 127 : q));
    }
  }
}

// ─────────────────────────────────────────────────────────────────────────────
// Preprocessing: RGB565 frame → centre crop → resize → int8 input
// ─────────────────────────────────────────────────────────────────────────────

static void preprocess(const camera_fb_t *fb, int8_t *input) {
  const int W = s_info.input_w, H = s_info.input_h, C = s_info.input_c;
  const int fw = fb->width;
  const uint8_t *src = fb->buf;

  for (int y = 0; y < H; y++) {
    // Nearest-neighbour: which source row/column lands on this pixel.
    const int sy = s_crop_y0 + (y * s_crop_h) / H;
    for (int x = 0; x < W; x++) {
      const int sx = s_crop_x0 + (x * s_crop_w) / W;
      const uint8_t *p = src + 2 * (sy * fw + sx);

      // esp32-camera stores RGB565 big-endian: RRRRRGGG GGGBBBBB
      const uint16_t px = (uint16_t)((p[0] << 8) | p[1]);
      const uint8_t r5 = px >> 11, g6 = (px >> 5) & 0x3F, b5 = px & 0x1F;
      // Expand to 8 bits by repeating the top bits (so 0x1F → 0xFF, not 0xF8)
      const uint8_t r = (r5 << 3) | (r5 >> 2);
      const uint8_t g = (g6 << 2) | (g6 >> 4);
      const uint8_t b = (b5 << 3) | (b5 >> 2);

      const int i = (y * W + x) * C;
      if (C == 3) {
        s_image[i + 0] = r;
        s_image[i + 1] = g;
        s_image[i + 2] = b;
        input[i + 0] = s_lut[0][r];
        input[i + 1] = s_lut[1][g];
        input[i + 2] = s_lut[2][b];
      } else {
        // ITU-R BT.601 luma in fixed point (weights sum to 256)
        const uint8_t l = (uint8_t)((77 * r + 150 * g + 29 * b) >> 8);
        s_image[i] = l;
        input[i] = s_lut[0][l];
      }
    }
  }
}

// ─────────────────────────────────────────────────────────────────────────────
// Public API
// ─────────────────────────────────────────────────────────────────────────────

extern "C" int fomo_init(const fomo_config_t *cfg, fomo_info_t *info) {
  if (!cfg || !info || !cfg->model_data || !cfg->norm_mean || !cfg->norm_std)
    return FOMO_ERR_ARGS;
  if (((uintptr_t)cfg->model_data & 15) != 0) {
    ESP_LOGE(TAG, "model data is not 16-byte aligned");
    return FOMO_ERR_ARGS;
  }

  log_heap("before model");

  s_model = tflite::GetModel(cfg->model_data);
  if (s_model->version() != TFLITE_SCHEMA_VERSION) {
    ESP_LOGE(TAG, "model schema v%lu, firmware expects v%d",
             (unsigned long)s_model->version(), TFLITE_SCHEMA_VERSION);
    return FOMO_ERR_MODEL_VERSION;
  }

  register_ops();
  int err = probe_arena(cfg->arena_probe_bytes);
  if (err)
    return err;

  if ((err = read_tensor_layout()))
    return err;
  if (cfg->norm_len < (size_t)s_info.input_c) {
    ESP_LOGE(TAG, "metadata has %u normalisation values, model has %d channels",
             (unsigned)cfg->norm_len, (int)s_info.input_c);
    return FOMO_ERR_ARGS;
  }
  build_lut(cfg);

  // Copy of the model input for the web view. Big, rarely read: PSRAM.
  size_t image_bytes = s_info.input_w * s_info.input_h * s_info.input_c;
  s_image = (uint8_t *)heap_caps_malloc(image_bytes, MALLOC_CAP_SPIRAM);
  if (!s_image)
    s_image = (uint8_t *)malloc(image_bytes);
  if (!s_image)
    return FOMO_ERR_ARENA_ALLOC;

  if ((err = camera_init(s_info.input_w, s_info.input_h, cfg->vflip, cfg->hmirror)))
    return err;
  log_heap("after camera");

  if ((err = place_arena(cfg->internal_reserve_bytes)))
    return err;
  log_heap("after arena");

  // Centre crop with the model's aspect ratio (square for 120×120, 5:3 for
  // 160×96). Squashing the whole 4:3 frame into the input instead would
  // stretch objects in a way the model never saw in training.
  const int fw = s_info.camera_w, fh = s_info.camera_h;
  const int W = s_info.input_w, H = s_info.input_h;
  if (fw * H >= fh * W) { // frame is wider than the model: trim the sides
    s_crop_h = fh;
    s_crop_w = fh * W / H;
  } else { // frame is taller: trim top and bottom
    s_crop_w = fw;
    s_crop_h = fw * H / W;
  }
  s_crop_x0 = (fw - s_crop_w) / 2;
  s_crop_y0 = (fh - s_crop_h) / 2;
  ESP_LOGI(TAG, "using a %dx%d crop at (%d,%d) of each %dx%d frame", s_crop_w,
           s_crop_h, s_crop_x0, s_crop_y0, fw, fh);

  *info = s_info;
  return 0;
}

extern "C" int fomo_run(fomo_timing_t *timing) {
  if (!s_interpreter || !timing)
    return FOMO_ERR_ARGS;

  int64_t t = esp_timer_get_time();
  camera_fb_t *fb = esp_camera_fb_get();
  if (!fb)
    return FOMO_ERR_CAPTURE;
  timing->capture_us = elapsed_us(t);

  if (fb->format != PIXFORMAT_RGB565 || fb->width != (size_t)s_info.camera_w ||
      fb->height != (size_t)s_info.camera_h) {
    esp_camera_fb_return(fb);
    return FOMO_ERR_FRAME_FORMAT;
  }

  t = esp_timer_get_time();
  preprocess(fb, s_interpreter->input(0)->data.int8);
  // Give the buffer back as soon as we're done so the DMA can refill it.
  esp_camera_fb_return(fb);
  timing->preprocess_us = elapsed_us(t);

  t = esp_timer_get_time();
  TfLiteStatus status = s_interpreter->Invoke();
  timing->invoke_us = elapsed_us(t);
  if (status != kTfLiteOk)
    return FOMO_ERR_INVOKE;
  return 0;
}

extern "C" const uint8_t *fomo_image(void) { return s_image; }

extern "C" const int8_t *fomo_output(void) {
  return s_interpreter ? s_interpreter->output(0)->data.int8 : nullptr;
}
