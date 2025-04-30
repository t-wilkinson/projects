use crate::netedit::proxy_connection;
use crate::proxy::pipe;
use rand;
use std::net::{Shutdown, TcpStream};

const NEVER_GONNA_GIVE_YOU_UP_NEVER_GONNA_LET_YOU_DOWN_NEVER_GONNA_RUN_AROUND_AND_DESERT_YOU: &str =
    "https://www.youtube.com/watch?v=dQw4w9WgXcQ";

pub fn rickroll_proxy(mut incoming_stream: TcpStream, dst_addr: &str) -> Result<(), String> {
    let mut rng = rand::thread_rng();

    if rng.gen::<f64>() < 0.3 {
        // Rick Roll
        println!("Get rick-rolled bruv");
        incoming_stream.write_all(
            format!("HTTP/1.1 301 Moved Permanently\r\nLocation: {}\r\n",
                NEVER_GONNA_GIVE_YOU_UP_NEVER_GONNA_LET_YOU_DOWN_NEVER_GONNA_RUN_AROUND_AND_DESERT_YOU
            ).as_bytes()
        ).map_err(|e| e.to_string())?;
        incoming_stream.shutdown(Shutdown::Both);
    } else {
        proxy_connection(incoming_stream, dst_addr);
    }

    Ok(())
}
