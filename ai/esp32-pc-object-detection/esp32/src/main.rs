// ─────────────────────────────────────────────────────────────────────────────
// FOMO object detection on the ESP32-S3 (and ESP32-CAM), viewed in a browser
//
//   C++  components/fomo  camera capture, preprocessing, TFLite Micro
//   Rust detect.rs        output tensor → scores → detections (host-testable)
//        inference.rs     the capture → infer → decode loop, on core 1
//        server.rs        HTTP API + web page, on core 0 with WiFi
// ─────────────────────────────────────────────────────────────────────────────

mod detect;
mod inference;
mod model;
mod server;
mod state;
mod wifi;

use std::{sync::Arc, thread, time::Duration};

use anyhow::Result;
use esp_idf_hal::peripherals::Peripherals;
use esp_idf_svc::{eventloop::EspSystemEventLoop, nvs::EspDefaultNvsPartition};
use log::info;

fn main() -> Result<()> {
    esp_idf_svc::sys::link_patches();
    esp_idf_svc::log::EspLogger::initialize_default();

    info!("FOMO object detection: {} byte model", model::model_bytes());

    let peripherals = Peripherals::take()?;
    let _wifi = wifi::connect(
        peripherals.modem,
        EspSystemEventLoop::take()?,
        EspDefaultNvsPartition::take()?,
    )?;

    let shared = Arc::new(state::Shared::new());
    let _server = server::start(shared.clone())?;

    // WiFi first, then the model: by the time the pipeline decides whether
    // the tensor arena fits in internal SRAM, WiFi has already taken its share.
    inference::spawn(shared)?;

    // `_wifi` and `_server` shut down when dropped, so main never returns.
    loop {
        thread::sleep(Duration::from_secs(60));
    }
}
