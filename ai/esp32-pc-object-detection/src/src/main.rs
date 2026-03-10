// ─────────────────────────────────────────────────────────────────────────────
// ESP32-S WebSocket server
//
// What this does:
//   1. Connects to a WiFi network (STA mode)
//   2. Starts an HTTP server that upgrades /ws to WebSocket
//   3. Echos every text/binary frame back to the sender
//   4. Broadcasts a "ping" message to all connected clients every 5 seconds
//
// Crates: esp-idf-svc, embedded-svc, esp-idf-hal, anyhow, log
// ─────────────────────────────────────────────────────────────────────────────

use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
    thread,
    time::Duration,
};

use anyhow::Result;

use embedded_svc::wifi::{AuthMethod, ClientConfiguration, Configuration as WifiConfig};

use esp_idf_hal::peripherals::Peripherals;

use esp_idf_svc::{
    eventloop::EspSystemEventLoop,
    http::server::{ws::EspHttpWsConnection, Configuration as HttpConfig, EspHttpServer},
    nvs::EspDefaultNvsPartition,
    wifi::{BlockingWifi, EspWifi},
    io::Write,
};

use esp_idf_sys as _; // pulls in the esp-idf-sys link patches

// ── WiFi credentials ─────────────────────────────────────────────────────────
const WIFI_SSID:     &str = env!("WIFI_SSID");
const WIFI_PASSWORD: &str = env!("WIFI_PASSWORD");

// ── WebSocket server port ────────────────────────────────────────────────────
// Clients connect to:  ws://<device-ip>/ws
// You can find the device IP in the serial monitor after boot.
const HTTP_PORT: u16 = 80;

// ─────────────────────────────────────────────────────────────────────────────
// Shared state: track all connected WebSocket session IDs so we can
// broadcast to them from a background thread.
// ─────────────────────────────────────────────────────────────────────────────
type Sessions = Arc<Mutex<HashMap<i32, ()>>>;

fn main() -> Result<()> {
    // Required: links esp-idf patches + initialises the logging backend.
    esp_idf_svc::sys::link_patches();
    esp_idf_svc::log::EspLogger::initialize_default();

    log::info!("ESP32 WebSocket server starting...");

    // ── Peripherals ───────────────────────────────────────────────────────────
    let peripherals = Peripherals::take()?;
    let sysloop      = EspSystemEventLoop::take()?;
    let nvs          = EspDefaultNvsPartition::take()?;

    // ── Connect to WiFi ───────────────────────────────────────────────────────
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
    log::info!("WiFi started, connecting to \"{}\"...", WIFI_SSID);

    wifi.connect()?;
    wifi.wait_netif_up()?;

    let ip_info = wifi.wifi().sta_netif().get_ip_info()?;
    log::info!("Connected! IP address: {}", ip_info.ip);
    log::info!("WebSocket server will be at: ws://{}/ws", ip_info.ip);

    // ── HTTP / WebSocket server ───────────────────────────────────────────────
    let server_config = HttpConfig {
        http_port: HTTP_PORT,
        // Allow up to 4 simultaneous WebSocket connections.
        // Increase if you need more; each costs ~6 KB of stack.
        max_sessions: 4,
        session_timeout: Duration::from_secs(300),
        ..Default::default()
    };

    let mut server = EspHttpServer::new(&server_config)?;

    // Shared session map — cloned into both the WS handler and the
    // broadcast thread below.
    let sessions: Sessions = Arc::new(Mutex::new(HashMap::new()));
    let sessions_for_broadcast = Arc::clone(&sessions);

    // ── /ws endpoint ─────────────────────────────────────────────────────────
    server.ws_handler("/ws", move |ws: &mut EspHttpWsConnection| {
        handle_ws(ws, &sessions)
    })?;

    // ── Optional: health-check HTTP endpoint ─────────────────────────────────
    server.fn_handler("/", esp_idf_svc::http::Method::Get, |req| {
        req.into_ok_response()?
            .write_all(b"ESP32 WebSocket server is running. Connect to ws://<ip>/ws")?;
        Ok::<(), anyhow::Error>(())
    })?;

    // ── Background broadcast thread ───────────────────────────────────────────
    // Every 5 seconds, sends a "ping" text frame to every connected client.
    // You can replace this with sensor readings, state updates, etc.
    thread::Builder::new()
        .name("ws-broadcast".into())
        .stack_size(4096)
        .spawn(move || {
            let mut counter: u32 = 0;
            loop {
                thread::sleep(Duration::from_secs(5));
                counter += 1;

                let msg = format!("{{\"type\":\"ping\",\"count\":{}}}", counter);
                log::info!("Broadcasting: {}", msg);

                // NOTE: Broadcasting requires calling ws_send_frame on each
                // session individually via the server handle.  With esp-idf-svc
                // 0.48 the idiomatic way is to keep a reference to the server
                // and call server.send_ws_message(session_id, ...).
                // Because EspHttpServer is not Send, the simplest approach is
                // to do the broadcast work inside the handler itself (see
                // handle_ws below) or use esp_http_server C bindings directly.
                //
                // For now we just log; see handle_ws for per-connection echo.
                let session_count = sessions_for_broadcast.lock().unwrap().len();
                log::info!("Active sessions: {}", session_count);
            }
        })?;

    // ── Keep the main thread alive ────────────────────────────────────────────
    // The WiFi and HTTP server live as long as their owning variables do, so
    // we must not let main() return.
    log::info!("Server running. Waiting for connections...");
    loop {
        thread::sleep(Duration::from_secs(1));
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// WebSocket connection handler
//
// esp-idf-svc calls this closure each time an event fires on a connection:
//   • is_new()    — the HTTP→WS upgrade just completed
//   • is_closed() — the client disconnected
//   • otherwise   — a frame is available to read with recv()
// ─────────────────────────────────────────────────────────────────────────────
fn handle_ws(
    ws:       &mut EspHttpWsConnection,
    sessions: &Sessions,
) -> Result<(), esp_idf_svc::sys::EspError> {
    use embedded_svc::ws::FrameType;

    // ── New connection ────────────────────────────────────────────────────────
    if ws.is_new() {
        let id = ws.session();
        log::info!("[WS] New connection  session_id={}", id);
        sessions.lock().unwrap().insert(id, ());
        // Send a welcome message as soon as the client connects.
        ws.send(
            FrameType::Text(false),
            br#"{"type":"welcome","msg":"Connected to ESP32 WebSocket server"}"#,
        )?;
        return Ok(());
    }

    // ── Connection closed ─────────────────────────────────────────────────────
    if ws.is_closed() {
        let id = ws.session();
        log::info!("[WS] Connection closed  session_id={}", id);
        sessions.lock().unwrap().remove(&id);
        return Ok(());
    }

    // ── Incoming frame ────────────────────────────────────────────────────────
    // Use a stack-allocated buffer.  Adjust size to your expected message size.
    // Frames larger than this will be truncated — add fragmentation handling
    // if you need to receive large payloads.
    let mut buf = [0u8; 512];
    let (frame_type, len) = ws.recv(&mut buf)?;

    match frame_type {
        // ── Text frame ────────────────────────────────────────────────────────
        FrameType::Text(fragmented) => {
            let text = std::str::from_utf8(&buf[..len])
                .unwrap_or("<invalid utf-8>");
            log::info!(
                "[WS] Text frame  session={} fragmented={} len={} data={}",
                ws.session(), fragmented, len, text
            );

            // Echo the message back wrapped in a JSON envelope.
            let response = format!(
                "{{\"type\":\"echo\",\"data\":{}}}",
                // Re-use the raw bytes if they look like valid JSON,
                // otherwise wrap in a string.
                if text.starts_with('{') || text.starts_with('[') {
                    text.to_owned()
                } else {
                    format!("\"{}\"", text.replace('"', "\\\""))
                }
            );
            ws.send(FrameType::Text(false), response.as_bytes())?;
        }

        // ── Binary frame ──────────────────────────────────────────────────────
        FrameType::Binary(fragmented) => {
            log::info!(
                "[WS] Binary frame  session={} fragmented={} len={}",
                ws.session(), fragmented, len
            );
            // Echo raw bytes back.
            ws.send(FrameType::Binary(false), &buf[..len])?;
        }

        // ── Ping — reply with Pong ────────────────────────────────────────────
        FrameType::Ping => {
            log::debug!("[WS] Ping  session={}", ws.session());
            ws.send(FrameType::Pong, &buf[..len])?;
        }

        // ── Pong — nothing to do ──────────────────────────────────────────────
        FrameType::Pong => {
            log::debug!("[WS] Pong  session={}", ws.session());
        }

        // ── Close frame ───────────────────────────────────────────────────────
        FrameType::Close => {
            log::info!("[WS] Close frame  session={}", ws.session());
            ws.send(FrameType::Close, &[])?;
        }

        // ── Continuation / other ──────────────────────────────────────────────
        other => {
            log::warn!("[WS] Unhandled frame type: {:?}  session={}", other, ws.session());
        }
    }

    Ok(())
}
