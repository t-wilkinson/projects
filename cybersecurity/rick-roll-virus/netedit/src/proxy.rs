use std::io::{Read, Write};
use std::net::{Shutdown, TcpStream};

const BUFFER_SIZE: usize = 128;

/// Pipe incoming TcpStream to outgoing TcpStream
pub fn pipe(incoming: &mut TcpStream, outgoing: &mut TcpStream) -> Result<(), String> {
    let mut buffer = [0; BUFFER_SIZE];
    loop {
        match incoming.read(&mut buffer) {
            Ok(bytes_read) => {
                // Done reading incoming stream
                if bytes_read == 0 || bytes_read < BUFFER_SIZE {
                    outgoing
                        .shutdown(Shutdown::Both)
                        .map_err(|e| format!("Error shutting down: {}", e))?;
                    break;
                }

                if outgoing.write(&buffer[..bytes_read]).is_ok() {
                    outgoing
                        .flush()
                        .map_err(|e| format!("Error writing buffer: {}", e))?;
                }
            }
            Err(e) => return Err(format!("Could not read data: {}", e)),
        }
    }

    Ok(())
}
