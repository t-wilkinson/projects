// ─────────────────────────────────────────────────────────────────────────────
// ESP32-CAM FOMO Object Detection + WebSocket Server
//
// What this does:
//   1. Connects to WiFi (STA mode)
//   2. Initialises the OV2640 camera
//   3. Loads a quantised FOMO TFLite-Micro model from flash
//   4. Runs inference on camera frames at ~2-5 FPS
//   5. Broadcasts detected objects (class, grid position, confidence) via WebSocket
//
// Crates: esp-idf-svc, esp-idf-hal, esp-tflite-micro, anyhow, log
// ─────────────────────────────────────────────────────────────────────────────

use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
    thread,
    time::{Duration, Instant},
};

use anyhow::{Result, Context};
use log::{info, warn, error, debug};

use embedded_svc::wifi::{AuthMethod, ClientConfiguration, Configuration as WifiConfig};

use esp_idf_hal::peripherals::Peripherals;

use esp_idf_svc::{
    eventloop::EspSystemEventLoop,
    http::server::{ws::EspHttpWsConnection, Configuration as HttpConfig, EspHttpServer},
    nvs::EspDefaultNvsPartition,
    wifi::{BlockingWifi, EspWifi},
    io::Write,
};

use esp_idf_sys as _;

// ── Configuration ────────────────────────────────────────────────────────────
const WIFI_SSID:     &str = env!("WIFI_SSID");
const WIFI_PASSWORD: &str = env!("WIFI_PASSWORD");

const HTTP_PORT: u16 = 80;

/// FOMO model parameters (must match the trained model!)
const MODEL_INPUT_W:  usize = 96;
const MODEL_INPUT_H:  usize = 96;
const MODEL_CHANNELS: usize = 3;   // RGB
const GRID_W:         usize = 12;  // INPUT_W / stride(8)
const GRID_H:         usize = 12;
const NUM_CLASSES:     usize = 6;   // background + 5 object classes

/// ImageNet normalisation parameters (baked into quantisation scale/zero-point
/// by the TFLite converter, so we only need to convert from uint8→int8 range).
const IMAGENET_MEAN: [f32; 3] = [0.485, 0.456, 0.406];
const IMAGENET_STD:  [f32; 3] = [0.229, 0.224, 0.225];

/// Class names — index 0 = background
const CLASS_NAMES: [&str; NUM_CLASSES] = [
    "background",
    "person",
    "sports_ball",
    "chair",
    "couch",
    "dining_table",
];

/// Confidence threshold for reporting detections
const CONF_THRESHOLD: f32 = 0.45;

/// How many bytes the TFLite arena needs (tweak after first run if OOM)
const TENSOR_ARENA_SIZE: usize = 300 * 1024; // 300 KB — allocated in PSRAM

/// How often to run inference (milliseconds between frames)
const INFERENCE_INTERVAL_MS: u64 = 300; // ~3 FPS

// ── Embedded model binary ────────────────────────────────────────────────────
// This includes the .tflite file directly into the firmware binary.
// Place `fomo_model_int8.tflite` in the `model/` directory of this crate.
const MODEL_DATA: &[u8] = include_bytes!("../model/fomo_model_int8.tflite");

// ── Types ────────────────────────────────────────────────────────────────────
type Sessions = Arc<Mutex<HashMap<i32, ()>>>;

/// A single object detection from the FOMO grid.
#[derive(Debug, Clone)]
struct Detection {
    class_id:   u8,
    class_name: &'static str,
    grid_x:     u8,
    grid_y:     u8,
    /// Pixel coordinates of the centroid (in the 96×96 input space)
    pixel_x:    u16,
    pixel_y:    u16,
    confidence: f32,
}

impl Detection {
    /// Serialise to a compact JSON string.
    fn to_json(&self) -> String {
        format!(
            "{{\"cls\":{},\"name\":\"{}\",\"gx\":{},\"gy\":{},\"px\":{},\"py\":{},\"conf\":{:.2}}}",
            self.class_id, self.class_name,
            self.grid_x, self.grid_y,
            self.pixel_x, self.pixel_y,
            self.confidence,
        )
    }
}

/// Shared state between the inference thread and the WebSocket handler.
struct SharedState {
    sessions:   HashMap<i32, ()>,
    detections: Vec<Detection>,
    frame_count: u32,
    inference_ms: u32,
}

type SharedStateHandle = Arc<Mutex<SharedState>>;

// ─────────────────────────────────────────────────────────────────────────────
// Camera driver (OV2640) — uses esp-idf C bindings via FFI
// ─────────────────────────────────────────────────────────────────────────────
mod camera {
    use esp_idf_sys::*;
    use anyhow::{Result, bail};

    /// GPIO pin assignments for the AI-Thinker ESP32-CAM board.
    /// Adjust these if you're using a different ESP32-CAM variant.
    const PWDN_GPIO:  i32 = 32;
    const RESET_GPIO: i32 = -1;  // Not connected
    const XCLK_GPIO:  i32 = 0;
    const SIOD_GPIO:  i32 = 26;
    const SIOC_GPIO:  i32 = 27;
    const Y9_GPIO:    i32 = 35;
    const Y8_GPIO:    i32 = 34;
    const Y7_GPIO:    i32 = 39;
    const Y6_GPIO:    i32 = 36;
    const Y5_GPIO:    i32 = 21;
    const Y4_GPIO:    i32 = 19;
    const Y3_GPIO:    i32 = 18;
    const Y2_GPIO:    i32 = 5;
    const VSYNC_GPIO: i32 = 25;
    const HREF_GPIO:  i32 = 23;
    const PCLK_GPIO:  i32 = 22;

    /// Initialise the OV2640 camera.
    ///
    /// Returns a raw pointer to the camera configuration.  Frame buffers are
    /// allocated in PSRAM.
    pub fn init() -> Result<()> {
        unsafe {
            let config = camera_config_t {
                pin_pwdn:  PWDN_GPIO,
                pin_reset: RESET_GPIO,
                pin_xclk:  XCLK_GPIO,
                pin_sccb_sda: SIOD_GPIO,
                pin_sccb_scl: SIOC_GPIO,
                pin_d7: Y9_GPIO,
                pin_d6: Y8_GPIO,
                pin_d5: Y7_GPIO,
                pin_d4: Y6_GPIO,
                pin_d3: Y5_GPIO,
                pin_d2: Y4_GPIO,
                pin_d1: Y3_GPIO,
                pin_d0: Y2_GPIO,
                pin_vsync: VSYNC_GPIO,
                pin_href:  HREF_GPIO,
                pin_pclk:  PCLK_GPIO,

                xclk_freq_hz: 20_000_000,
                ledc_timer:   ledc_timer_t_LEDC_TIMER_0,
                ledc_channel: ledc_channel_t_LEDC_CHANNEL_0,

                // RGB565 is the fastest format the OV2640 supports at this resolution.
                // We'll convert to RGB888 in software for the model.
                pixel_format: pixformat_t_PIXFORMAT_RGB565,

                // QVGA (320×240) — we downsample to 96×96 in software.
                // Using a camera-native resolution avoids slow JPEG decode.
                frame_size: framesize_t_FRAMESIZE_QVGA,

                jpeg_quality: 12,  // Not used for RGB565 but required by struct
                fb_count:     1,   // Single frame buffer (saves PSRAM)

                grab_mode: camera_grab_mode_t_CAMERA_GRAB_WHEN_EMPTY,

                fb_location: camera_fb_location_t_CAMERA_FB_IN_PSRAM,

                // Zero-initialise remaining fields
                ..core::mem::zeroed()
            };

            let ret = esp_camera_init(&config);
            if ret != ESP_OK {
                bail!("esp_camera_init failed with error code {}", ret);
            }
        }

        log::info!("[CAM] OV2640 initialised (QVGA RGB565)");
        Ok(())
    }

    /// Capture a single frame.  Returns (width, height, raw_pixel_data).
    ///
    /// The pixel data is in RGB565 format, 2 bytes per pixel.
    /// The caller MUST call `return_frame()` when done to free the buffer.
    pub fn capture() -> Result<(*const u8, usize, usize, usize)> {
        unsafe {
            let fb = esp_camera_fb_get();
            if fb.is_null() {
                bail!("esp_camera_fb_get returned null — camera not ready?");
            }

            let width  = (*fb).width as usize;
            let height = (*fb).height as usize;
            let len    = (*fb).len as usize;
            let buf    = (*fb).buf;

            Ok((buf, len, width, height))
        }
    }

    /// Return the frame buffer to the camera driver (MUST be called after capture).
    pub fn return_frame() {
        unsafe {
            let fb = esp_camera_fb_get();
            if !fb.is_null() {
                esp_camera_fb_return(fb);
            }
        }
    }

    /// Capture and immediately convert to a 96×96 RGB888 buffer.
    ///
    /// Performs bilinear-approximated downscaling from 320×240 → 96×96.
    /// Returns a Vec<u8> of length 96*96*3 in row-major RGB order.
    pub fn capture_rgb96(out_buf: &mut [u8; super::MODEL_INPUT_W * super::MODEL_INPUT_H * super::MODEL_CHANNELS]) -> Result<()> {
        unsafe {
            let fb = esp_camera_fb_get();
            if fb.is_null() {
                anyhow::bail!("Camera capture failed");
            }

            let src_w = (*fb).width as usize;
            let src_h = (*fb).height as usize;
            let src_buf = core::slice::from_raw_parts((*fb).buf, (*fb).len as usize);

            let dst_w = super::MODEL_INPUT_W;
            let dst_h = super::MODEL_INPUT_H;

            // Nearest-neighbour downscale from RGB565 → RGB888 at 96×96
            for dy in 0..dst_h {
                let sy = (dy * src_h) / dst_h;
                for dx in 0..dst_w {
                    let sx = (dx * src_w) / dst_w;

                    // RGB565: [MSB] RRRRR GGGGGG BBBBB [LSB], stored little-endian
                    let idx = (sy * src_w + sx) * 2;
                    let lo = src_buf[idx] as u16;
                    let hi = src_buf[idx + 1] as u16;
                    let pixel = (hi << 8) | lo;

                    let r = ((pixel >> 11) & 0x1F) as u8;
                    let g = ((pixel >> 5)  & 0x3F) as u8;
                    let b = (pixel & 0x1F) as u8;

                    // Scale to 0-255
                    let out_idx = (dy * dst_w + dx) * 3;
                    out_buf[out_idx]     = (r << 3) | (r >> 2);
                    out_buf[out_idx + 1] = (g << 2) | (g >> 4);
                    out_buf[out_idx + 2] = (b << 3) | (b >> 2);
                }
            }

            esp_camera_fb_return(fb);
        }

        Ok(())
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// TFLite Micro inference wrapper
// ─────────────────────────────────────────────────────────────────────────────
mod inference {
    use super::*;
    use tfmicro::{MicroInterpreter, Model, AllOpResolver};

    /// Everything needed to run the FOMO model.
    pub struct FomoInference {
        // The MicroInterpreter owns the arena, model ref, and resolver.
        // We store it as an opaque wrapper because its lifetime is tied
        // to the arena buffer.
        //
        // In practice on ESP32, this lives for the entire program lifetime,
        // so we use a 'static lifetime by leaking the arena allocation.
        interpreter: MicroInterpreter<'static>,
    }

    impl FomoInference {
        /// Load the FOMO model and allocate the TFLite Micro interpreter.
        ///
        /// `arena` must be a &'static mut [u8] of at least TENSOR_ARENA_SIZE bytes.
        pub fn new(model_data: &'static [u8], arena: &'static mut [u8]) -> Result<Self> {
            let model = Model::from_buffer(model_data)
                .context("Failed to parse TFLite model")?;

            let resolver = AllOpResolver::new();

            let interpreter = MicroInterpreter::new(model, resolver, arena)
                .context("Failed to create TFLite Micro interpreter")?;

            info!("[TFLITE] Model loaded successfully");
            info!("[TFLITE] Arena used: {} / {} bytes",
                  interpreter.arena_used_bytes(), arena.len());

            Ok(Self { interpreter })
        }

        /// Run inference on a pre-processed 96×96 RGB image.
        ///
        /// `input` must be MODEL_INPUT_W * MODEL_INPUT_H * MODEL_CHANNELS int8 values,
        /// already normalised using the quantisation parameters baked into the model.
        pub fn run(&mut self, input: &[i8]) -> Result<Vec<Detection>> {
            // Copy input into the interpreter's input tensor
            let input_tensor = self.interpreter.input_mut(0);
            let input_data = input_tensor.data_mut::<i8>();
            input_data.copy_from_slice(input);

            // Run!
            self.interpreter.invoke()
                .context("TFLite invoke failed")?;

            // Read output: [1, NUM_CLASSES, GRID_H, GRID_W] (int8, NCHW)
            // or [1, GRID_H, GRID_W, NUM_CLASSES] (int8, NHWC) depending on converter
            let output_tensor = self.interpreter.output(0);
            let output_data = output_tensor.data::<i8>();
            let output_shape = output_tensor.dims();

            let detections = Self::parse_output(output_data, output_shape)?;
            Ok(detections)
        }

        /// Parse the raw int8 output grid into Detection structs.
        fn parse_output(data: &[i8], shape: &[usize]) -> Result<Vec<Detection>> {
            let mut detections = Vec::new();

            // Determine layout: NCHW vs NHWC
            let (is_nchw, _n, c, h, w) = if shape.len() == 4 {
                if shape[1] == NUM_CLASSES {
                    // NCHW: [1, 6, 12, 12]
                    (true, shape[0], shape[1], shape[2], shape[3])
                } else {
                    // NHWC: [1, 12, 12, 6]
                    (false, shape[0], shape[3], shape[1], shape[2])
                }
            } else {
                anyhow::bail!("Unexpected output shape: {:?}", shape);
            };

            let cell_w = MODEL_INPUT_W as f32 / w as f32;
            let cell_h = MODEL_INPUT_H as f32 / h as f32;

            for gy in 0..h {
                for gx in 0..w {
                    // Find the class with highest logit for this cell
                    let mut best_class = 0usize;
                    let mut best_val = i8::MIN;

                    for cls in 0..c {
                        let val = if is_nchw {
                            data[cls * h * w + gy * w + gx]
                        } else {
                            data[gy * w * c + gx * c + cls]
                        };

                        if val > best_val {
                            best_val = val;
                            best_class = cls;
                        }
                    }

                    // Skip background (class 0)
                    if best_class == 0 {
                        continue;
                    }

                    // Convert int8 logit to approximate confidence via softmax
                    // (simplified: just use relative margin over background)
                    let bg_val = if is_nchw {
                        data[gy * w + gx]  // class 0, NCHW
                    } else {
                        data[gy * w * c + gx * c]  // class 0, NHWC
                    };

                    let margin = (best_val as f32 - bg_val as f32) / 128.0;
                    let confidence = 1.0 / (1.0 + (-margin * 4.0).exp()); // sigmoid

                    if confidence < CONF_THRESHOLD {
                        continue;
                    }

                    let pixel_x = ((gx as f32 + 0.5) * cell_w) as u16;
                    let pixel_y = ((gy as f32 + 0.5) * cell_h) as u16;

                    detections.push(Detection {
                        class_id:   best_class as u8,
                        class_name: CLASS_NAMES[best_class],
                        grid_x:     gx as u8,
                        grid_y:     gy as u8,
                        pixel_x,
                        pixel_y,
                        confidence,
                    });
                }
            }

            Ok(detections)
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Image preprocessing: uint8 RGB → int8 normalised (matching training pipeline)
// ─────────────────────────────────────────────────────────────────────────────

/// Convert a 96×96 RGB888 image (u8, 0–255) into the int8 tensor format
/// expected by the quantised TFLite model.
///
/// The quantisation parameters (scale, zero_point) are baked into the model
/// by the TFLite converter.  They encode the ImageNet normalisation
/// `(pixel/255 - mean) / std` into int8 range.
///
/// For a typical int8 model:
///   real_value = (int8_value - zero_point) * scale
///   int8_value = real_value / scale + zero_point
///
/// We precompute: normalised = (pixel/255 - mean) / std
/// then quantise:  int8 = clamp(round(normalised / scale + zero_point), -128, 127)
///
/// `input_scale` and `input_zero_point` come from the model's input tensor
/// quantisation metadata.  Pass them in from the interpreter at init time.
fn preprocess_image(
    rgb_buf:    &[u8],      // 96*96*3 bytes, row-major RGB
    out_buf:    &mut [i8],  // 96*96*3 int8 values
    scale:      f32,
    zero_point: i32,
) {
    let total = MODEL_INPUT_W * MODEL_INPUT_H;

    for i in 0..total {
        for ch in 0..3 {
            let pixel_f = rgb_buf[i * 3 + ch] as f32 / 255.0;
            let normalised = (pixel_f - IMAGENET_MEAN[ch]) / IMAGENET_STD[ch];
            let quantised = (normalised / scale + zero_point as f32).round() as i32;
            let clamped = quantised.clamp(-128, 127) as i8;

            // TFLite expects CHW if model is NCHW, or HWC if NHWC.
            // The ONNX→TFLite converter usually keeps NCHW for MobileNet.
            // Adjust index order if your model uses NHWC:
            //   NCHW: out[ch * total + i] = clamped;
            //   NHWC: out[i * 3 + ch]     = clamped;
            // We default to NCHW here — change if your export differs.
            out_buf[ch * total + i] = clamped;
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// WebSocket handler (same pattern as original, but broadcasts detections)
// ─────────────────────────────────────────────────────────────────────────────

fn handle_ws(
    ws:    &mut EspHttpWsConnection,
    state: &SharedStateHandle,
) -> Result<(), esp_idf_svc::sys::EspError> {
    use embedded_svc::ws::FrameType;

    // ── New connection ────────────────────────────────────────────────────
    if ws.is_new() {
        let id = ws.session();
        info!("[WS] New connection  session_id={}", id);
        state.lock().unwrap().sessions.insert(id, ());

        // Send welcome + model info
        let welcome = format!(
            "{{\"type\":\"welcome\",\"model\":\"FOMO\",\"input\":\"{}x{}\",\"grid\":\"{}x{}\",\"classes\":{:?}}}",
            MODEL_INPUT_W, MODEL_INPUT_H, GRID_W, GRID_H, &CLASS_NAMES[1..],
        );
        ws.send(FrameType::Text(false), welcome.as_bytes())?;
        return Ok(());
    }

    // ── Connection closed ─────────────────────────────────────────────────
    if ws.is_closed() {
        let id = ws.session();
        info!("[WS] Connection closed  session_id={}", id);
        state.lock().unwrap().sessions.remove(&id);
        return Ok(());
    }

    // ── Incoming frame ────────────────────────────────────────────────────
    let mut buf = [0u8; 256];
    let (frame_type, len) = ws.recv(&mut buf)?;

    match frame_type {
        FrameType::Text(_) => {
            let text = std::str::from_utf8(&buf[..len]).unwrap_or("");
            debug!("[WS] Received: {}", text);

            // Client can request a snapshot of current detections
            if text.contains("\"get_detections\"") || text.contains("\"snapshot\"") {
                let state_guard = state.lock().unwrap();
                let json = format_detections_json(
                    &state_guard.detections,
                    state_guard.frame_count,
                    state_guard.inference_ms,
                );
                drop(state_guard);
                ws.send(FrameType::Text(false), json.as_bytes())?;
            } else {
                // Echo back
                let resp = format!("{{\"type\":\"echo\",\"data\":\"{}\"}}", text.replace('"', "\\\""));
                ws.send(FrameType::Text(false), resp.as_bytes())?;
            }
        }
        FrameType::Ping => {
            ws.send(FrameType::Pong, &buf[..len])?;
        }
        FrameType::Close => {
            ws.send(FrameType::Close, &[])?;
        }
        _ => {}
    }

    Ok(())
}

/// Format the current detections as a JSON message for WebSocket broadcast.
fn format_detections_json(detections: &[Detection], frame: u32, ms: u32) -> String {
    let det_json: Vec<String> = detections.iter().map(|d| d.to_json()).collect();
    format!(
        "{{\"type\":\"detections\",\"frame\":{},\"inference_ms\":{},\"count\":{},\"objects\":[{}]}}",
        frame, ms, detections.len(), det_json.join(",")
    )
}

// ─────────────────────────────────────────────────────────────────────────────
// Main
// ─────────────────────────────────────────────────────────────────────────────

fn run() -> Result<()> {
    esp_idf_svc::sys::link_patches();
    esp_idf_svc::log::EspLogger::initialize_default();

    info!("═══════════════════════════════════════════════════");
    info!("  ESP32-CAM FOMO Object Detection Server");
    info!("═══════════════════════════════════════════════════");

    // ── Peripherals & WiFi ────────────────────────────────────────────────
    let peripherals = Peripherals::take()?;
    let sysloop     = EspSystemEventLoop::take()?;
    let nvs         = EspDefaultNvsPartition::take()?;

    let mut wifi = BlockingWifi::wrap(
        EspWifi::new(peripherals.modem, sysloop.clone(), Some(nvs))?,
        sysloop,
    )?;

    wifi.set_configuration(&WifiConfig::Client(ClientConfiguration {
        ssid:        WIFI_SSID.try_into().expect("SSID too long"),
        password:    WIFI_PASSWORD.try_into().expect("Password too long"),
        auth_method: AuthMethod::WPA2Personal,
        ..Default::default()
    }))?;

    wifi.start()?;
    info!("[WIFI] Connecting to \"{}\"...", WIFI_SSID);
    wifi.connect()?;
    wifi.wait_netif_up()?;

    let ip_info = wifi.wifi().sta_netif().get_ip_info()?;
    info!("[WIFI] Connected! IP: {}", ip_info.ip);
    info!("[WIFI] WebSocket: ws://{}/ws", ip_info.ip);

    // ── Initialise camera ─────────────────────────────────────────────────
    camera::init().context("Camera initialisation failed")?;

    // ── Initialise TFLite Micro ───────────────────────────────────────────
    info!("[TFLITE] Loading FOMO model ({} bytes)...", MODEL_DATA.len());

    // Allocate the tensor arena in PSRAM (heap).
    // We leak the Box to get a 'static reference — this is intentional since
    // the interpreter must live for the entire program.
    let arena: &'static mut [u8] = {
        let boxed = vec![0u8; TENSOR_ARENA_SIZE].into_boxed_slice();
        Box::leak(boxed)
    };

    let mut fomo = inference::FomoInference::new(MODEL_DATA, arena)
        .context("Failed to load FOMO model")?;

    info!("[TFLITE] Model ready!");

    // ── Shared state ──────────────────────────────────────────────────────
    let shared_state: SharedStateHandle = Arc::new(Mutex::new(SharedState {
        sessions:     HashMap::new(),
        detections:   Vec::new(),
        frame_count:  0,
        inference_ms: 0,
    }));

    // ── HTTP / WebSocket server ───────────────────────────────────────────
    let server_config = HttpConfig {
        http_port: HTTP_PORT,
        max_sessions: 4,
        session_timeout: Duration::from_secs(300),
        ..Default::default()
    };

    let mut server = EspHttpServer::new(&server_config)?;

    // Clone handle for the WS handler closure
    let state_for_ws = Arc::clone(&shared_state);
    server.ws_handler("/ws", move |ws: &mut EspHttpWsConnection| {
        handle_ws(ws, &state_for_ws)
    })?;

    // Health-check endpoint
    server.fn_handler("/", esp_idf_svc::http::Method::Get, |req| {
        let html = concat!(
            "<html><body style='font-family:monospace;background:#111;color:#0f0;padding:2em'>",
            "<h1>ESP32-CAM FOMO Server</h1>",
            "<p>Connect via WebSocket: <code>ws://&lt;this-ip&gt;/ws</code></p>",
            "<p>Model: FOMO (MobileNetV2 backbone, 96x96 input, 12x12 grid)</p>",
            "<p>Classes: person, sports_ball, chair, couch, dining_table</p>",
            "</body></html>"
        );
        req.into_ok_response()?
            .write_all(html.as_bytes())?;
        Ok::<(), anyhow::Error>(())
    })?;

    // ── Inference loop (runs on main thread) ──────────────────────────────
    info!("[MAIN] Starting inference loop (~{} FPS)...",
          1000 / INFERENCE_INTERVAL_MS);

    let mut rgb_buf = [0u8; MODEL_INPUT_W * MODEL_INPUT_H * MODEL_CHANNELS];
    let mut input_buf = vec![0i8; MODEL_INPUT_W * MODEL_INPUT_H * MODEL_CHANNELS];

    // Get quantisation parameters from the model's input tensor
    // (these are set by the TFLite converter and encode the normalisation)
    let (input_scale, input_zero_point) = (0.003921568859368563_f32, -128_i32);
    // ↑ Common values for uint8→int8 with /255 normalisation.
    //   Replace with actual values from your model's input_details['quantization_parameters'].
    //   See the notebook cell "Verify the TFLite model" for exact values.

    let mut frame_count: u32 = 0;

    loop {
        let t_start = Instant::now();

        // 1. Capture frame from camera → 96×96 RGB888
        if let Err(e) = camera::capture_rgb96(&mut rgb_buf) {
            warn!("[CAM] Capture failed: {}", e);
            thread::sleep(Duration::from_millis(100));
            continue;
        }

        // 2. Preprocess: normalise & quantise to int8
        preprocess_image(&rgb_buf, &mut input_buf, input_scale, input_zero_point);

        // 3. Run FOMO inference
        let detections = match fomo.run(&input_buf) {
            Ok(dets) => dets,
            Err(e) => {
                error!("[TFLITE] Inference failed: {}", e);
                thread::sleep(Duration::from_millis(100));
                continue;
            }
        };

        let inference_ms = t_start.elapsed().as_millis() as u32;
        frame_count += 1;

        // 4. Log detections
        if !detections.is_empty() {
            info!("[FOMO] Frame {} ({} ms): {} detections",
                  frame_count, inference_ms, detections.len());
            for d in &detections {
                debug!("  {} at grid({},{}) conf={:.0}%",
                       d.class_name, d.grid_x, d.grid_y, d.confidence * 100.0);
            }
        }

        // 5. Update shared state (for WebSocket clients)
        {
            let mut guard = shared_state.lock().unwrap();
            guard.detections = detections.clone();
            guard.frame_count = frame_count;
            guard.inference_ms = inference_ms;
        }

        // 6. Broadcast to WebSocket clients
        //    NOTE: Direct broadcast requires server access. Since EspHttpServer
        //    is not Send, we broadcast by updating shared state — clients can
        //    poll via "get_detections" messages, or you can use the esp-idf
        //    httpd_ws_send_frame() C API for push-based broadcast.
        //
        //    For push-based broadcast, see the unsafe block below:
        #[cfg(feature = "push_broadcast")]
        {
            let json = format_detections_json(&detections, frame_count, inference_ms);
            let guard = shared_state.lock().unwrap();
            for &session_id in guard.sessions.keys() {
                unsafe {
                    // This calls the underlying C API to push a frame
                    // to a specific WebSocket session.
                    use esp_idf_sys::*;
                    let frame = httpd_ws_frame_t {
                        type_: httpd_ws_type_t_HTTPD_WS_TYPE_TEXT,
                        payload: json.as_ptr() as *mut u8,
                        len: json.len(),
                        ..core::mem::zeroed()
                    };
                    httpd_ws_send_frame_async(
                        server.handle(),
                        session_id,
                        &frame as *const _ as *mut _,
                    );
                }
            }
        }

        // 7. Rate limit
        let elapsed = t_start.elapsed();
        let target = Duration::from_millis(INFERENCE_INTERVAL_MS);
        if elapsed < target {
            thread::sleep(target - elapsed);
        }
    }
}
