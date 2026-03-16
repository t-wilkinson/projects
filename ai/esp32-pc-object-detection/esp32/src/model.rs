use std::{thread, time::Duration};

use anyhow::{bail, Result};
use log::{debug, info, warn};

// ── Configuration ────────────────────────────────────────────────────────────

const TENSOR_ARENA_SIZE: usize = 486 * 1024;

/// Class names — index 0 = background (never returned by fomo_detect)
pub const CLASS_NAMES: [&str; 6] = [
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
pub struct FomoDetectionC {
    class_id: u8,
    grid_x: u8,
    grid_y: u8,
    _pad: u8,
    pixel_x: u16,
    pixel_y: u16,
    confidence: f32,
}

#[repr(C)]
pub struct FomoResultC {
    detections: [FomoDetectionC; FOMO_MAX_DETECTIONS],
    pub count: i32,
    pub inference_ms: u32,
}

extern "C" {
    fn fomo_init(
        model_data: *const u8,
        model_data_len: usize,
        arena_size_bytes: usize,
    ) -> i32;

    fn fomo_detect(out: *mut FomoResultC) -> i32;

    fn fomo_get_bmp(out_len: *mut i32) -> *const u8;
}

// ── Rust-side detection type ─────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct Detection {
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

pub fn format_detections_json(detections: &[Detection], frame: u32, ms: u32) -> String {
    let det_json: Vec<String> = detections.iter().map(|d| d.to_json()).collect();
    format!(
        "{{\"type\":\"detections\",\"frame\":{},\"inference_ms\":{},\"count\":{},\"objects\":[{}]}}",
        frame,
        ms,
        detections.len(),
        det_json.join(",")
    )
}

// ── Public API ───────────────────────────────────────────────────────────────

/// Returns BMP bytes of the last captured frame
pub fn get_snapshot_bmp() -> &'static [u8] {
    let mut len: i32 = 0;
    unsafe {
        let ptr = fomo_get_bmp(&mut len);
        std::slice::from_raw_parts(ptr, len as usize)
    }
}

pub fn setup() -> Result<(), anyhow::Error> {
    info!(
        "[FOMO] Initialising camera + model ({} bytes)...",
        MODEL_DATA.len()
    );
    let ret = unsafe { fomo_init(MODEL_DATA.as_ptr(), MODEL_DATA.len(), TENSOR_ARENA_SIZE) };
    if ret != 0 {
        bail!("fomo_init failed (error={})", ret);
    }
    info!("[FOMO] Pipeline ready!");
    Ok(())
}

pub fn inference(result: &mut FomoResultC, frame_count: u32) -> Result<Vec<Detection>> {
    let ret = unsafe { fomo_detect(result) };
    if ret != 0 {
        warn!("[FOMO] fomo_detect failed (error={})", ret);
        thread::sleep(Duration::from_millis(100));
        return Ok(vec![]);
    }

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
    Ok(detections)
}
