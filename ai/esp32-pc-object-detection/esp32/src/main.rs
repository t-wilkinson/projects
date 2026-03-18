#![allow(unused_variables)]
// ─────────────────────────────────────────────────────────────────────────────
// ESP32-CAM FOMO Object Detection + WebSocket Server
//
// Architecture:
//   C/C++ side  → camera capture, preprocessing, TFLite Micro inference
//   Rust side   → WiFi, HTTP server, WebSocket broadcast
//
// The FFI surface is two functions: fomo_init() and fomo_detect().
// ─────────────────────────────────────────────────────────────────────────────

mod model;
mod server;

use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
    thread,
    time::{Duration, Instant},
};

use anyhow::Result;
use log::{error, info};
use esp_idf_svc::sys::httpd_handle_t;
use esp_idf_hal::task::thread::ThreadSpawnConfiguration;

use crate::model::{Detection, FomoResultC};

// ── Configuration ────────────────────────────────────────────────────────────

pub const WIFI_SSID: &str = env!("WIFI_SSID");
pub const WIFI_PASSWORD: &str = env!("WIFI_PASSWORD");
pub const HTTP_PORT: u16 = 80;

const INFERENCE_INTERVAL_MS: u64 = 300; // ~3 FPS

// ── Shared state ─────────────────────────────────────────────────────────────

pub struct SendableHandle(pub httpd_handle_t);
unsafe impl Send for SendableHandle {}
unsafe impl Sync for SendableHandle {}

pub struct SharedState {
    server_handle: Option<SendableHandle>,
    pub sessions: HashMap<i32, ()>,
    pub detections: Vec<Detection>,
    pub frame_count: u32,
    pub inference_ms: u32,
}

pub type SharedStateHandle = Arc<Mutex<SharedState>>;


// ── Main ─────────────────────────────────────────────────────────────────────

fn run() -> Result<()> {
    esp_idf_svc::sys::link_patches();
    esp_idf_svc::log::EspLogger::initialize_default();

    info!("═══════════════════════════════════════════════════");
    info!("  ESP32-CAM FOMO Object Detection Server");
    info!("═══════════════════════════════════════════════════");

    let shared_state: SharedStateHandle = Arc::new(Mutex::new(SharedState {
        server_handle: None,
        sessions: HashMap::new(),
        detections: Vec::new(),
        frame_count: 0,
        inference_ms: 0,
    }));

    let _server = server::init(Arc::clone(&shared_state))?;

    // Initialise camera + ML
    model::setup()?;

    // ── Inference loop ───────────────────────────────────────────────────
    info!(
        "[MAIN] Starting inference loop (~{} FPS)...",
        1000 / INFERENCE_INTERVAL_MS
    );

    let mut frame_count: u32 = 0;
    let mut raw_result = unsafe { core::mem::zeroed::<FomoResultC>() };

    loop {
        let t_start = Instant::now();

        // let detections = match model::inference(&mut raw_result, frame_count) {
        //     Ok(v) => v,
        //     Err(_) => continue,
        // };

        frame_count += 1;

        // Update shared state for WS clients
        // {
        //     let mut guard = shared_state.lock().unwrap();
        //     guard.detections = detections;
        //     guard.frame_count = frame_count;
        //     guard.inference_ms = raw_result.inference_ms;
        // }

        // server::broadcast_detections(&shared_state);

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
