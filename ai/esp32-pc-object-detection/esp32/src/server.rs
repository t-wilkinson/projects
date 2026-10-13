// ─────────────────────────────────────────────────────────────────────────────
// server.rs — HTTP endpoints for the browser view
//
//   GET /               the web page (web/index.html, baked into the firmware)
//   GET /api/info       model, tensor arena and camera details (JSON)
//   GET /api/frame      latest frame: [u32 LE json len][json][image][scores]
//   GET /api/settings   ?threshold=0.4&vflip=1&hmirror=0 → current settings
//
// The page polls /api/frame: it asks for the next frame only after drawing
// the last one. A slow phone simply gets fewer frames; nothing queues up on
// the ESP32 the way pushed WebSocket messages would.
// ─────────────────────────────────────────────────────────────────────────────

use std::sync::{atomic::Ordering, Arc};

use anyhow::Result;
use esp_idf_svc::{
    http::{
        server::{Configuration, EspHttpConnection, EspHttpServer, Request},
        Method,
    },
    io::Write,
};

use crate::state::Shared;

const INDEX_HTML: &str = include_str!("../web/index.html");

// Lets you open web/index.html from your PC (?device=http://<ip>) while
// iterating on the page, without reflashing.
const CORS: (&str, &str) = ("Access-Control-Allow-Origin", "*");

pub fn start(shared: Arc<Shared>) -> Result<EspHttpServer<'static>> {
    let mut server = EspHttpServer::new(&Configuration {
        // Browsers open several connections; recycle the oldest instead of
        // refusing new ones.
        max_open_sockets: 7,
        lru_purge_enable: true,
        ..Default::default()
    })?;

    server.fn_handler("/", Method::Get, |req| {
        req.into_response(200, None, &[("Content-Type", "text/html; charset=utf-8")])?
            .write_all(INDEX_HTML.as_bytes())?;
        Ok::<(), anyhow::Error>(())
    })?;

    let s = shared.clone();
    server.fn_handler("/api/info", Method::Get, move |req| {
        let body = s.info_json.lock().unwrap().clone();
        send_json(req, &body)
    })?;

    let s = shared.clone();
    server.fn_handler("/api/frame", Method::Get, move |req| {
        // Clone the Arc and drop the lock before the (slow) network write.
        let frame = s.latest_frame.lock().unwrap().clone();
        match frame {
            Some(body) => {
                req.into_response(
                    200,
                    None,
                    &[
                        ("Content-Type", "application/octet-stream"),
                        ("Cache-Control", "no-store"),
                        CORS,
                    ],
                )?
                .write_all(&body)?;
            }
            None => {
                req.into_response(503, Some("No frame yet"), &[CORS])?;
            }
        }
        Ok::<(), anyhow::Error>(())
    })?;

    let s = shared;
    server.fn_handler("/api/settings", Method::Get, move |req| {
        let query = req.uri().split_once('?').map_or("", |(_, q)| q).to_owned();
        let (mut vflip, mut hmirror) = s.orientation();
        for (key, value) in query.split('&').filter_map(|kv| kv.split_once('=')) {
            match key {
                "threshold" => {
                    if let Ok(t) = value.parse::<f32>() {
                        s.threshold_bits
                            .store(t.clamp(0.01, 1.0).to_bits(), Ordering::Relaxed);
                    }
                }
                "vflip" => vflip = value == "1",
                "hmirror" => hmirror = value == "1",
                _ => {}
            }
        }
        s.set_orientation(vflip, hmirror);
        send_json(req, &s.settings_json())
    })?;

    Ok(server)
}

fn send_json(req: Request<&mut EspHttpConnection>, body: &str) -> Result<()> {
    req.into_response(
        200,
        None,
        &[
            ("Content-Type", "application/json"),
            ("Cache-Control", "no-store"),
            CORS,
        ],
    )?
    .write_all(body.as_bytes())?;
    Ok(())
}
