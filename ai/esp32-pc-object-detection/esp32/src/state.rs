// ─────────────────────────────────────────────────────────────────────────────
// state.rs — what the inference thread and the HTTP handlers share
// ─────────────────────────────────────────────────────────────────────────────

use std::sync::{
    atomic::{AtomicBool, AtomicU32, Ordering},
    Arc, Mutex,
};

use esp_idf_svc::sys::{
    heap_caps_get_free_size, heap_caps_get_largest_free_block, heap_caps_get_minimum_free_size,
    MALLOC_CAP_8BIT, MALLOC_CAP_INTERNAL, MALLOC_CAP_SPIRAM,
};

use crate::model::CONF_THRESHOLD;

pub struct Shared {
    /// Latest encoded /api/frame body. The inference thread swaps in a new
    /// Arc each frame; a handler clones the Arc and sends it without holding
    /// the lock, so a slow client never blocks inference.
    pub latest_frame: Mutex<Option<Arc<Vec<u8>>>>,
    /// /api/info body; holds {"error": ...} if the pipeline failed to start.
    pub info_json: Mutex<String>,
    /// Detection threshold as f32 bits, adjustable from the browser.
    pub threshold_bits: AtomicU32,
    vflip: AtomicBool,
    hmirror: AtomicBool,
}

impl Shared {
    pub fn new() -> Self {
        Self {
            latest_frame: Mutex::new(None),
            info_json: Mutex::new("{\"status\":\"starting\"}".into()),
            threshold_bits: AtomicU32::new(CONF_THRESHOLD.to_bits()),
            vflip: AtomicBool::new(false),
            hmirror: AtomicBool::new(false),
        }
    }

    pub fn orientation(&self) -> (bool, bool) {
        (
            self.vflip.load(Ordering::Relaxed),
            self.hmirror.load(Ordering::Relaxed),
        )
    }

    pub fn set_orientation(&self, vflip: bool, hmirror: bool) {
        self.vflip.store(vflip, Ordering::Relaxed);
        self.hmirror.store(hmirror, Ordering::Relaxed);
    }

    pub fn settings_json(&self) -> String {
        let (vflip, hmirror) = self.orientation();
        format!(
            "{{\"threshold\":{:.2},\"vflip\":{vflip},\"hmirror\":{hmirror}}}",
            f32::from_bits(self.threshold_bits.load(Ordering::Relaxed)),
        )
    }
}

/// Free memory right now. `internal_min` is the low-water mark since boot:
/// if it gets near zero, WiFi will start failing to allocate packets.
pub fn heap_json() -> String {
    unsafe {
        format!(
            "{{\"internal_free\":{},\"internal_largest\":{},\"internal_min\":{},\"psram_free\":{}}}",
            heap_caps_get_free_size(MALLOC_CAP_INTERNAL),
            heap_caps_get_largest_free_block(MALLOC_CAP_INTERNAL | MALLOC_CAP_8BIT),
            heap_caps_get_minimum_free_size(MALLOC_CAP_INTERNAL),
            heap_caps_get_free_size(MALLOC_CAP_SPIRAM),
        )
    }
}
