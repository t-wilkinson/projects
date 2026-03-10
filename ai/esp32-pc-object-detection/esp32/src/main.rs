// ─────────────────────────────────────────────────────────────────────────────
// ESP32-CAM FOMO Object Detection + WebSocket Server
//
// Architecture:
//   C/C++ side  → camera capture, preprocessing, TFLite Micro inference
//   Rust side   → WiFi, HTTP server, WebSocket broadcast
//
// The FFI surface is two functions: fomo_init() and fomo_detect().
// ─────────────────────────────────────────────────────────────────────────────

use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
    thread,
    time::{Duration, Instant},
};

use anyhow::{bail, Context, Result};
use log::{debug, error, info, warn};

use embedded_svc::wifi::{AuthMethod, ClientConfiguration, Configuration as WifiConfig};

use esp_idf_hal::peripherals::Peripherals;

use esp_idf_svc::{
    eventloop::EspSystemEventLoop,
    http::server::{ws::EspHttpWsConnection, Configuration as HttpConfig, EspHttpServer},
    io::Write,
    nvs::EspDefaultNvsPartition,
    wifi::{BlockingWifi, EspWifi},
};

use esp_idf_sys as _;

// ── Configuration ────────────────────────────────────────────────────────────

const WIFI_SSID: &str = env!("WIFI_SSID");
const WIFI_PASSWORD: &str = env!("WIFI_PASSWORD");

const HTTP_PORT: u16 = 80;
const INFERENCE_INTERVAL_MS: u64 = 300; // ~3 FPS
const TENSOR_ARENA_SIZE: usize = 300 * 1024;

/// Class names — index 0 = background (never returned by fomo_detect)
const CLASS_NAMES: [&str; 6] = [
    "background",
    "person",
    "sports_ball",
    "chair",
    "couch",
    "dining_table",
];

// ── Embedded model binary ────────────────────────────────────────────────────
const MODEL_DATA: &[u8] = include_bytes!("../model/fomo_model_int8.tflite");

// ── FFI to the C/C++ FOMO component ─────────────────────────────────────────

const FOMO_MAX_DETECTIONS: usize = 32;

#[repr(C)]
#[derive(Clone, Copy, Debug)]
struct FomoDetectionC {
    class_id: u8,
    grid_x: u8,
    grid_y: u8,
    _pad: u8,
    pixel_x: u16,
    pixel_y: u16,
    confidence: f32,
}

#[repr(C)]
struct FomoResultC {
    detections: [FomoDetectionC; FOMO_MAX_DETECTIONS],
    count: i32,
    inference_ms: u32,
}

extern "C" {
    fn fomo_init(
        model_data: *const u8,
        model_data_len: usize,
        arena_size_bytes: usize,
    ) -> i32;

    fn fomo_detect(out: *mut FomoResultC) -> i32;
}

// ── Rust-side detection type ─────────────────────────────────────────────────

#[derive(Debug, Clone)]
struct Detection {
    class_id: u8,
    class_name: &'static str,
    grid_x: u8,
    grid_y: u8,
    pixel_x: u16,
    pixel_y: u16,
    confidence: f32,
}

impl Detection {
    fn from_c(c: &FomoDetectionC) -> Self {
        Self {
            class_id: c.class_id,
            class_name: CLASS_NAMES.get(c.class_id as usize).unwrap_or(&"unknown"),
            grid_x: c.grid_x,
            grid_y: c.grid_y,
            pixel_x: c.pixel_x,
            pixel_y: c.pixel_y,
            confidence: c.confidence,
        }
    }

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

// ── Shared state ─────────────────────────────────────────────────────────────

struct SharedState {
    sessions: HashMap<i32, ()>,
    detections: Vec<Detection>,
    frame_count: u32,
    inference_ms: u32,
}

type SharedStateHandle = Arc<Mutex<SharedState>>;

// ── WebSocket handler ────────────────────────────────────────────────────────

fn handle_ws(
    ws: &mut EspHttpWsConnection,
    state: &SharedStateHandle,
) -> Result<(), esp_idf_svc::sys::EspError> {
    use embedded_svc::ws::FrameType;

    if ws.is_new() {
        let id = ws.session();
        info!("[WS] New connection  session_id={}", id);
        state.lock().unwrap().sessions.insert(id, ());

        let welcome = format!(
            "{{\"type\":\"welcome\",\"model\":\"FOMO\",\"input\":\"96x96\",\"grid\":\"12x12\",\"classes\":{:?}}}",
            &CLASS_NAMES[1..],
        );
        ws.send(FrameType::Text(false), welcome.as_bytes())?;
        return Ok(());
    }

    if ws.is_closed() {
        let id = ws.session();
        info!("[WS] Connection closed  session_id={}", id);
        state.lock().unwrap().sessions.remove(&id);
        return Ok(());
    }

    let mut buf = [0u8; 256];
    let (frame_type, len) = ws.recv(&mut buf)?;

    match frame_type {
        FrameType::Text(_) => {
            let text = std::str::from_utf8(&buf[..len]).unwrap_or("");
            debug!("[WS] Received: {}", text);

            if text.contains("\"get_detections\"") || text.contains("\"snapshot\"") {
                let guard = state.lock().unwrap();
                let json = format_detections_json(
                    &guard.detections,
                    guard.frame_count,
                    guard.inference_ms,
                );
                drop(guard);
                ws.send(FrameType::Text(false), json.as_bytes())?;
            } else {
                let resp = format!(
                    "{{\"type\":\"echo\",\"data\":\"{}\"}}",
                    text.replace('"', "\\\"")
                );
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

fn format_detections_json(detections: &[Detection], frame: u32, ms: u32) -> String {
    let det_json: Vec<String> = detections.iter().map(|d| d.to_json()).collect();
    format!(
        "{{\"type\":\"detections\",\"frame\":{},\"inference_ms\":{},\"count\":{},\"objects\":[{}]}}",
        frame,
        ms,
        detections.len(),
        det_json.join(",")
    )
}

// ── Main ─────────────────────────────────────────────────────────────────────

fn run() -> Result<()> {
    esp_idf_svc::sys::link_patches();
    esp_idf_svc::log::EspLogger::initialize_default();

    info!("═══════════════════════════════════════════════════");
    info!("  ESP32-CAM FOMO Object Detection Server");
    info!("═══════════════════════════════════════════════════");

    // ── WiFi ─────────────────────────────────────────────────────────────
    let peripherals = Peripherals::take()?;
    let sysloop = EspSystemEventLoop::take()?;
    let nvs = EspDefaultNvsPartition::take()?;

    let mut wifi = BlockingWifi::wrap(
        EspWifi::new(peripherals.modem, sysloop.clone(), Some(nvs))?,
        sysloop,
    )?;

    wifi.set_configuration(&WifiConfig::Client(ClientConfiguration {
        ssid: WIFI_SSID.try_into().expect("SSID too long"),
        password: WIFI_PASSWORD.try_into().expect("Password too long"),
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

    // ── Initialise camera + ML (single C call) ──────────────────────────
    info!("[FOMO] Initialising camera + model ({} bytes)...", MODEL_DATA.len());

    let ret = unsafe {
        fomo_init(MODEL_DATA.as_ptr(), MODEL_DATA.len(), TENSOR_ARENA_SIZE)
    };
    if ret != 0 {
        bail!("fomo_init failed (error={})", ret);
    }
    info!("[FOMO] Pipeline ready!");

    // ── Shared state ─────────────────────────────────────────────────────
    let shared_state: SharedStateHandle = Arc::new(Mutex::new(SharedState {
        sessions: HashMap::new(),
        detections: Vec::new(),
        frame_count: 0,
        inference_ms: 0,
    }));

    // ── HTTP / WebSocket server ──────────────────────────────────────────
    let server_config = HttpConfig {
        http_port: HTTP_PORT,
        max_sessions: 4,
        session_timeout: Duration::from_secs(300),
        ..Default::default()
    };

    let mut server = EspHttpServer::new(&server_config)?;

    let state_for_ws = Arc::clone(&shared_state);
    server.ws_handler("/ws", move |ws: &mut EspHttpWsConnection| {
        handle_ws(ws, &state_for_ws)
    })?;

    server.fn_handler("/", esp_idf_svc::http::Method::Get, |req| {
        let html = concat!(
            "<html><body style='font-family:monospace;background:#111;color:#0f0;padding:2em'>",
            "<h1>ESP32-CAM FOMO Server</h1>",
            "<p>Connect via WebSocket: <code>ws://&lt;this-ip&gt;/ws</code></p>",
            "<p>Model: FOMO (MobileNetV2 backbone, 96x96 input, 12x12 grid)</p>",
            "<p>Classes: person, sports_ball, chair, couch, dining_table</p>",
            "</body></html>"
        );
        req.into_ok_response()?.write_all(html.as_bytes())?;
        Ok::<(), anyhow::Error>(())
    })?;

    // ── Inference loop ───────────────────────────────────────────────────
    info!(
        "[MAIN] Starting inference loop (~{} FPS)...",
        1000 / INFERENCE_INTERVAL_MS
    );

    let mut frame_count: u32 = 0;

    // Allocate the result struct once (stays on the stack)
    let mut result = unsafe { core::mem::zeroed::<FomoResultC>() };

    loop {
        let t_start = Instant::now();

        // One call: capture + preprocess + inference + parse
        let ret = unsafe { fomo_detect(&mut result) };
        if ret != 0 {
            warn!("[FOMO] fomo_detect failed (error={})", ret);
            thread::sleep(Duration::from_millis(100));
            continue;
        }

        frame_count += 1;

        // Convert C detections → Rust
        let count = result.count.max(0) as usize;
        let detections: Vec<Detection> = result.detections[..count]
            .iter()
            .map(Detection::from_c)
            .collect();

        if !detections.is_empty() {
            info!(
                "[FOMO] Frame {} ({} ms): {} detections",
                frame_count, result.inference_ms, detections.len()
            );
            for d in &detections {
                debug!(
                    "  {} at grid({},{}) conf={:.0}%",
                    d.class_name, d.grid_x, d.grid_y, d.confidence * 100.0
                );
            }
        }

        // Update shared state for WS clients
        {
            let mut guard = shared_state.lock().unwrap();
            guard.detections = detections;
            guard.frame_count = frame_count;
            guard.inference_ms = result.inference_ms;
        }

        // Rate limit
        let elapsed = t_start.elapsed();
        let target = Duration::from_millis(INFERENCE_INTERVAL_MS);
        if elapsed < target {
            thread::sleep(target - elapsed);
        }
    }
}

fn main() {
    if let Err(e) = run() {
        error!("Fatal error: {:?}", e);
        // Let the watchdog reset us
        loop {
            thread::sleep(Duration::from_secs(1));
        }
    }
}
