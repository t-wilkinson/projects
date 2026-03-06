use esp_idf_svc::eventloop::EspSystemEventLoop;
use esp_idf_svc::hal::prelude::Peripherals;
use esp_idf_svc::wifi::{AuthMethod, ClientConfiguration, Configuration, EspWifi};
use esp_idf_svc::http::server::{Configuration as HttpConfig, EspHttpServer, ws::EspHttpWsProcessor};
use embedded_svc::ws::FrameType;
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    esp_idf_svc::sys::link_patches();

    let peripherals = Peripherals::take()?;
    let sys_loop = EspSystemEventLoop::take()?;
    let nvs = esp_idf_svc::nvs::EspDefaultNvsPartition::take()?;

    // --- Wi-Fi Setup ---
    let mut wifi = EspWifi::new(peripherals.modem, sys_loop, Some(nvs))?;
    wifi.set_configuration(&Configuration::Client(ClientConfiguration {
        ssid: "YOUR_SSID".into(),
        password: "YOUR_PASSWORD".into(),
        auth_method: AuthMethod::WPA2WPA3Personal,
        ..Default::default()
    }))?;

    wifi.start()?;
    wifi.connect()?;
    println!("Wifi connected!");

    // --- WebSocket Server Setup ---
    let mut server = EspHttpServer::new(&HttpConfig::default())?;

    // WebSocket handler on "/ws"
    server.ws_handler("/ws", EspHttpWsProcessor::new(move |receiver, mut sender| {
        println!("New WS Connection established");
        loop {
            let (frame_type, len) = receiver.recv()?;
            let mut buf = vec![0u8; len];
            receiver.read(&mut buf)?;

            match frame_type {
                FrameType::Text(_) => {
                    let msg = std::str::from_utf8(&buf)?;
                    println!("Received: {}", msg);
                    sender.send(FrameType::Text(false), format!("Echo: {}", msg).as_bytes())?;
                }
                FrameType::Close => break,
                _ => {}
            }
        }
        Ok(())
    }))?;

    println!("WebSocket server running on /ws");

    // Keep the main thread alive
    loop {
        std::thread::sleep(std::time::Duration::from_secs(1));
    }
}
