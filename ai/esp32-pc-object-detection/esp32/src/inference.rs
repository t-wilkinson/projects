// ─────────────────────────────────────────────────────────────────────────────
// inference.rs — the capture → infer → decode loop, pinned to CPU core 1
//
// Core 0 runs the WiFi driver, lwIP and the HTTP server. Giving inference its
// own core means a slow browser never stalls the model and a long Invoke()
// never starves the network stack.
// ─────────────────────────────────────────────────────────────────────────────

use std::{
    fmt::Write as _,
    sync::{atomic::Ordering, Arc},
    thread,
    time::{Duration, Instant},
};

use anyhow::Result;
use esp_idf_hal::{cpu::Core, task::thread::ThreadSpawnConfiguration};
use log::{error, info, warn};

use crate::detect::{Activation, Decoder, Detection, OutputTensor};
use crate::model::{self, Pipeline, CLASS_NAMES, OUTPUT_ACTIVATION};
use crate::state::{heap_json, Shared};

const STACK_BYTES: usize = 16 * 1024;
const MAX_DETECTIONS: usize = 16;

pub fn spawn(shared: Arc<Shared>) -> Result<()> {
    ThreadSpawnConfiguration {
        name: Some(b"inference\0"),
        stack_size: STACK_BYTES,
        priority: 5,
        pin_to_core: Some(Core::Core1),
        ..Default::default()
    }
    .set()?;

    thread::Builder::new()
        .stack_size(STACK_BYTES)
        .spawn(move || {
            if let Err(e) = run(&shared) {
                error!("[infer] {e:#}");
                *shared.info_json.lock().unwrap() = format!("{{\"error\":{:?}}}", format!("{e:#}"));
            }
        })?;

    // Threads spawned after this get the default configuration again.
    ThreadSpawnConfiguration::default().set()?;
    Ok(())
}

fn run(shared: &Shared) -> Result<()> {
    let (mut vflip, mut hmirror) = shared.orientation();
    let mut pipeline = Pipeline::init(vflip, hmirror)?;
    let i = pipeline.info;

    let decoder = Decoder::new(
        OutputTensor {
            grid_w: i.grid_w as usize,
            grid_h: i.grid_h as usize,
            channels: i.output_c as usize,
            scale: i.output_scale,
            zero_point: i.output_zero_point,
        },
        CLASS_NAMES,
        OUTPUT_ACTIVATION.and_then(Activation::parse),
    )
    .map_err(anyhow::Error::msg)?;

    let info_json = describe(&pipeline, &decoder);
    info!("[infer] {info_json}");
    *shared.info_json.lock().unwrap() = info_json;

    let mut scores = vec![0u8; decoder.score_len()];
    let mut frame: u32 = 0;
    let mut window_start = Instant::now();
    let mut window_frames = 0u32;
    let mut fps = 0.0f32;

    loop {
        let wanted = shared.orientation();
        if wanted != (vflip, hmirror) {
            (vflip, hmirror) = wanted;
            pipeline.set_orientation(vflip, hmirror);
        }

        let timing = match pipeline.run() {
            Ok(t) => t,
            Err(e) => {
                warn!("[infer] {e:#}");
                thread::sleep(Duration::from_millis(100));
                continue;
            }
        };

        let t = Instant::now();
        decoder.scores(pipeline.output(), &mut scores);
        let threshold = f32::from_bits(shared.threshold_bits.load(Ordering::Relaxed));
        let detections = decoder.peaks(&scores, threshold, MAX_DETECTIONS);
        let decode_us = t.elapsed().as_micros() as u32;

        frame = frame.wrapping_add(1);
        window_frames += 1;
        let window = window_start.elapsed();
        if window >= Duration::from_secs(2) {
            fps = window_frames as f32 / window.as_secs_f32();
            info!(
                "[infer] {fps:.1} fps | capture {} ms, preprocess {} ms, invoke {} ms, decode {} ms | {} detections",
                timing.capture_us / 1000,
                timing.preprocess_us / 1000,
                timing.invoke_us / 1000,
                decode_us / 1000,
                detections.len(),
            );
            window_start = Instant::now();
            window_frames = 0;
        }

        let json = frame_json(frame, fps, threshold, &timing, decode_us, &detections);
        let body = encode_frame(&json, pipeline.image(), &scores);
        *shared.latest_frame.lock().unwrap() = Some(Arc::new(body));

        // Let lower-priority tasks on this core run (and the idle task feed
        // the watchdog) between frames.
        thread::sleep(Duration::from_millis(1));
    }
}

/// /api/frame body: [u32 LE json length][json][image bytes][score bytes].
/// One response carries the picture and the scores computed from it, so the
/// browser never overlays detections on a different frame.
fn encode_frame(json: &str, image: &[u8], scores: &[u8]) -> Vec<u8> {
    let mut body = Vec::with_capacity(4 + json.len() + image.len() + scores.len());
    body.extend_from_slice(&(json.len() as u32).to_le_bytes());
    body.extend_from_slice(json.as_bytes());
    body.extend_from_slice(image);
    body.extend_from_slice(scores);
    body
}

fn frame_json(
    frame: u32,
    fps: f32,
    threshold: f32,
    t: &model::Timing,
    decode_us: u32,
    detections: &[Detection],
) -> String {
    let mut s = format!(
        "{{\"frame\":{frame},\"fps\":{fps:.2},\"threshold\":{threshold:.2},\
         \"timing_us\":{{\"capture\":{},\"preprocess\":{},\"invoke\":{},\"decode\":{decode_us}}},\
         \"heap\":{},\"detections\":[",
        t.capture_us,
        t.preprocess_us,
        t.invoke_us,
        heap_json(),
    );
    for (n, d) in detections.iter().enumerate() {
        let _ = write!(
            s,
            "{}{{\"cls\":{},\"name\":{:?},\"gx\":{},\"gy\":{},\"x\":{:.4},\"y\":{:.4},\"score\":{:.3}}}",
            if n > 0 { "," } else { "" },
            d.class_id,
            CLASS_NAMES.get(d.class_id).copied().unwrap_or("?"),
            d.grid_x,
            d.grid_y,
            d.x,
            d.y,
            d.score,
        );
    }
    s.push_str("]}");
    s
}

/// Everything about the model and memory layout, for /api/info.
fn describe(p: &Pipeline, d: &Decoder) -> String {
    let i = &p.info;
    let names: Vec<&str> = CLASS_NAMES
        .iter()
        .copied()
        .filter(|n| *n != "background")
        .collect();
    format!(
        "{{\"chip\":{:?},\"model_bytes\":{},\
         \"input\":{{\"w\":{},\"h\":{},\"c\":{},\"scale\":{},\"zero_point\":{}}},\
         \"grid\":{{\"w\":{},\"h\":{},\"channels\":{},\"scale\":{},\"zero_point\":{}}},\
         \"classes\":{:?},\"activation\":{:?},\
         \"arena\":{{\"used\":{},\"allocated\":{},\"in\":{:?}}},\
         \"camera\":{{\"w\":{},\"h\":{},\"pid\":\"0x{:04x}\"}},\
         \"heap\":{}}}",
        env!("MCU"),
        model::model_bytes(),
        i.input_w,
        i.input_h,
        i.input_c,
        i.input_scale,
        i.input_zero_point,
        i.grid_w,
        i.grid_h,
        i.output_c,
        i.output_scale,
        i.output_zero_point,
        names,
        d.activation().name(),
        i.arena_used_bytes,
        i.arena_bytes,
        if i.arena_in_psram {
            "psram"
        } else {
            "internal"
        },
        i.camera_w,
        i.camera_h,
        i.camera_pid,
        heap_json(),
    )
}
