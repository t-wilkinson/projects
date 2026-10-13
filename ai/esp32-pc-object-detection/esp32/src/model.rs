// ─────────────────────────────────────────────────────────────────────────────
// model.rs — safe wrapper around the C++ camera + TFLite Micro pipeline
// ─────────────────────────────────────────────────────────────────────────────

use anyhow::{bail, Result};
use esp_idf_svc::sys::fomo;

include!(concat!(env!("OUT_DIR"), "/model_meta.rs"));

// ── Embedded model ───────────────────────────────────────────────────────────
// include_bytes! only guarantees 1-byte alignment. TFLM reads int32 biases
// and weights directly out of this buffer, and the Xtensa cores raise a
// LoadStoreAlignment exception on a misaligned 32-bit load, so wrap it in a
// 16-byte-aligned struct. The bytes stay in flash (.rodata) and are read
// through the cache; they are never copied into RAM.
#[repr(C, align(16))]
struct Aligned<T: ?Sized>(T);
static MODEL: &Aligned<[u8]> = &Aligned(*include_bytes!("../model/fomo_model_int8.tflite"));

pub fn model_bytes() -> usize {
    MODEL.0.len()
}

/// The first arena is a deliberately generous PSRAM allocation; the pipeline
/// measures what the model really needs and moves it to internal SRAM if it
/// fits. Raise this if boot fails with "AllocateTensors() failed".
const ARENA_PROBE_BYTES: usize = 1024 * 1024;

/// Internal SRAM that must stay free after placing the arena there. WiFi,
/// lwIP and the HTTP server keep allocating from it while running.
const INTERNAL_RESERVE_BYTES: usize = 64 * 1024;

pub type Info = fomo::fomo_info_t;
pub type Timing = fomo::fomo_timing_t;

pub struct Pipeline {
    pub info: Info,
}

impl Pipeline {
    pub fn init(vflip: bool, hmirror: bool) -> Result<Self> {
        let config = fomo::fomo_config_t {
            model_data: MODEL.0.as_ptr(),
            model_len: MODEL.0.len(),
            arena_probe_bytes: ARENA_PROBE_BYTES,
            internal_reserve_bytes: INTERNAL_RESERVE_BYTES,
            norm_mean: NORM_MEAN.as_ptr(),
            norm_std: NORM_STD.as_ptr(),
            norm_len: NORM_MEAN.len(),
            vflip,
            hmirror,
        };
        let mut info: Info = unsafe { core::mem::zeroed() };
        let ret = unsafe { fomo::fomo_init(&config, &mut info) };
        if ret != 0 {
            bail!(
                "fomo_init failed: {} ({ret}); see the serial log above",
                describe(ret)
            );
        }
        Ok(Self { info })
    }

    /// Capture a frame and run the model on it.
    pub fn run(&mut self) -> Result<Timing> {
        let mut timing: Timing = unsafe { core::mem::zeroed() };
        let ret = unsafe { fomo::fomo_run(&mut timing) };
        if ret != 0 {
            bail!("fomo_run failed: {} ({ret})", describe(ret));
        }
        Ok(timing)
    }

    /// Pixels the model saw on the last run (H × W × C, 0–255).
    pub fn image(&self) -> &[u8] {
        let i = &self.info;
        let len = (i.input_w * i.input_h * i.input_c) as usize;
        unsafe { core::slice::from_raw_parts(fomo::fomo_image(), len) }
    }

    /// Raw int8 output tensor of the last run (grid H × grid W × channels).
    pub fn output(&self) -> &[i8] {
        let i = &self.info;
        let len = (i.grid_w * i.grid_h * i.output_c) as usize;
        unsafe { core::slice::from_raw_parts(fomo::fomo_output(), len) }
    }

    pub fn set_orientation(&mut self, vflip: bool, hmirror: bool) {
        unsafe { fomo::fomo_set_orientation(vflip, hmirror) }
    }
}

fn describe(code: i32) -> &'static str {
    match code {
        -1 => "bad arguments (is the model 16-byte aligned?)",
        -2 => "camera init failed (check the ribbon cable and pin map)",
        -3 => "model schema version mismatch",
        -4 => "out of memory for tensor arena or image buffer",
        -5 => "AllocateTensors failed (arena too small or unsupported op)",
        -6 => "unexpected tensor type/shape (needs int8 NHWC in and out)",
        -7 => "camera capture failed",
        -8 => "camera frame has the wrong format or size",
        -9 => "Invoke failed",
        _ => "unknown error",
    }
}
