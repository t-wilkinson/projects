use anyhow::Result;
use esp_idf_hal::modem::Modem;
use esp_idf_svc::{
    eventloop::EspSystemEventLoop,
    nvs::EspDefaultNvsPartition,
    wifi::{AuthMethod, BlockingWifi, ClientConfiguration, Configuration, EspWifi},
};
use log::info;

// Set at compile time:  WIFI_SSID=... WIFI_PASSWORD=... cargo run --release
const SSID: &str = env!("WIFI_SSID");
const PASSWORD: &str = env!("WIFI_PASSWORD");

/// Join the WiFi network. Keep the returned value alive: dropping it
/// disconnects.
pub fn connect(
    modem: Modem,
    sysloop: EspSystemEventLoop,
    nvs: EspDefaultNvsPartition,
) -> Result<BlockingWifi<EspWifi<'static>>> {
    let mut wifi = BlockingWifi::wrap(EspWifi::new(modem, sysloop.clone(), Some(nvs))?, sysloop)?;

    wifi.set_configuration(&Configuration::Client(ClientConfiguration {
        ssid: SSID.try_into().expect("WIFI_SSID too long"),
        password: PASSWORD.try_into().expect("WIFI_PASSWORD too long"),
        auth_method: if PASSWORD.is_empty() {
            AuthMethod::None
        } else {
            AuthMethod::WPA2Personal
        },
        ..Default::default()
    }))?;

    wifi.start()?;
    info!("[wifi] connecting to {SSID:?}...");
    wifi.connect()?;
    wifi.wait_netif_up()?;

    let ip = wifi.wifi().sta_netif().get_ip_info()?.ip;
    info!("[wifi] connected. Open http://{ip}/ in a browser");
    Ok(wifi)
}
