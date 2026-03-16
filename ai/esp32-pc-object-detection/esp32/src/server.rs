use std::sync::Arc;
use std::time::Duration;

use anyhow::Result;
use embedded_svc::wifi::{AuthMethod, ClientConfiguration, Configuration as WifiConfig};
use embedded_svc::ws::FrameType;
use esp_idf_hal::peripherals::Peripherals;
use esp_idf_svc::{
    eventloop::EspSystemEventLoop,
    http::server::{ws::EspHttpWsConnection, Configuration as HttpConfig, EspHttpServer},
    http::Method,
    io::Write,
    nvs::EspDefaultNvsPartition,
    wifi::{BlockingWifi, EspWifi},
};
use esp_idf_svc::handle::RawHandle;
use esp_idf_svc::sys::{
    httpd_ws_frame_t, httpd_ws_send_frame_async,
    httpd_ws_type_t_HTTPD_WS_TYPE_TEXT,
};
use log::{debug, info, warn};

use crate::model::{self, format_detections_json, CLASS_NAMES};
use crate::{SendableHandle, SharedStateHandle, HTTP_PORT, WIFI_PASSWORD, WIFI_SSID};

fn handle_ws(
    ws: &mut EspHttpWsConnection,
    state: &SharedStateHandle,
) -> Result<(), esp_idf_svc::sys::EspError> {
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
                let json = model::format_detections_json(
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

pub fn broadcast_detections(state: &SharedStateHandle) {
    let guard = state.lock().unwrap();

    let handle = match &guard.server_handle {
        Some(h) => h.0,
        None => return,
    };

    if guard.sessions.is_empty() {
        return;
    }

    let json = format_detections_json(&guard.detections, guard.frame_count, guard.inference_ms);
    let payload = json.as_bytes();

    let sessions: Vec<i32> = guard.sessions.keys().cloned().collect();
    drop(guard); // release lock before doing I/O

    for id in &sessions {
        let mut frame = httpd_ws_frame_t {
            type_: httpd_ws_type_t_HTTPD_WS_TYPE_TEXT,
            payload: payload.as_ptr() as *mut u8,
            len: payload.len(),
            ..unsafe { core::mem::zeroed() }
        };

        let ret = unsafe { httpd_ws_send_frame_async(handle, *id, &mut frame) };

        if ret != 0 {
            warn!(
                "[WS] Push to session_id={} failed (err={}), removing",
                id, ret
            );
            if let Ok(mut g) = state.lock() {
                g.sessions.remove(id);
            }
        }
    }
}

pub fn init(shared_state: SharedStateHandle) -> Result<EspHttpServer<'static>> {
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

    // ── HTTP / WebSocket server ──────────────────────────────────────────
    let server_config = HttpConfig {
        http_port: HTTP_PORT,
        max_sessions: 4,
        session_timeout: Duration::from_secs(300),
        ..Default::default()
    };

    let mut server = EspHttpServer::new(&server_config)?;

    // Store server handle so broadcast_detections can use it
    {
        let mut guard = shared_state.lock().unwrap();
        guard.server_handle = Some(SendableHandle(server.handle()));
    }

    let state_for_ws = Arc::clone(&shared_state);
    server.ws_handler("/ws", move |ws: &mut EspHttpWsConnection| {
        handle_ws(ws, &state_for_ws)
    })?;

    // Keep wifi alive by leaking it (standard esp-idf-svc pattern).
    // The wifi driver must outlive the server; Box::leak ensures it is
    // never dropped while the device is running.
    Box::leak(Box::new(wifi));

    server.fn_handler("/", Method::Get, |req| {
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

    server.fn_handler("/snapshot", Method::Get, |req| {
        let bmp = crate::model::get_snapshot_bmp();

        let headers = [
            ("Content-Type", "image/bmp"),
            ("Cache-Control", "no-cache"),
        ];
        let mut resp = req.into_response(200, None, &headers)?;
        resp.write_all(bmp)?;
        Ok::<(), anyhow::Error>(())
    })?;

    server.fn_handler("/stream", Method::Get, |req| {
        let mut resp = req.into_response(
            200,
            None,
            &[("Content-Type", "multipart/x-mixed-replace; boundary=frame")],
        )?;

        loop {
            let bmp = crate::model::get_snapshot_bmp();
            let header = format!(
                "--frame\r\nContent-Type: image/bmp\r\nContent-Length: {}\r\n\r\n",
                bmp.len()
            );
            if resp.write_all(header.as_bytes()).is_err() {
                break;
            }
            if resp.write_all(bmp).is_err() {
                break;
            }
            if resp.write_all(b"\r\n").is_err() {
                break;
            }
            std::thread::sleep(std::time::Duration::from_millis(300));
        }
        Ok::<(), anyhow::Error>(())
    })?;

    Ok(server)
}
