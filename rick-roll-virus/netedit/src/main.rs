#![allow(unused)]
use rand::Rng;
use std::env;
use std::fs::File;
use std::io::{self, Error, ErrorKind, Read, Write};
use std::net::{Shutdown, TcpListener, TcpStream};
use std::process;
use std::sync::Arc;
use std::thread::{spawn, JoinHandle};

const BUFFER_SIZE: usize = 128;
const NEVER_GONNA_GIVE_YOU_UP_NEVER_GONNA_LET_YOU_DOWN_NEVER_GONNA_RUN_AROUND_AND_DESERT_YOU: &str =
    "https://www.youtube.com/watch?v=dQw4w9WgXcQ";

fn help() {
    println!("Usage: netedit <protocol> <port> <dst-host:dst-port>");
}

enum Protocol {
    TCP,
    UDP,
}

struct NetEdit {
    protocol: Protocol,
    port: u16,
    dst_host: String,
    dst_port: String,
}

fn parse_args(args: Vec<String>) -> Result<NetEdit, String> {
    if args.len() != 4 {
        return Err("Invalid number of arguments".to_owned());
    }

    let protocol = &args[1];
    let protocol = match protocol.to_lowercase().as_str() {
        "tcp" => Protocol::TCP,
        "udp" => Protocol::UDP,
        _ => return Err("Protocol must be one of `tcp` or `udp`".to_owned()),
    };

    let port = match args[2].parse::<u16>() {
        Ok(port) => port,
        Err(err) => return Err(format!("{}", err)),
    };

    let destination = &args[3];
    let destination: Vec<&str> = destination.split(':').collect();
    if destination.len() != 2 {
        eprintln!("Destination address must be in the form <host:port>");
        help();
        process::exit(1);
    };

    return Ok(NetEdit {
        protocol,
        port,
        dst_host: String::from(destination[0]),
        dst_port: String::from(destination[1]),
    });
}

fn pipe(incoming: &mut TcpStream, outgoing: &mut TcpStream) -> Result<(), String> {
    let mut buffer = [0; BUFFER_SIZE];
    loop {
        match incoming.read(&mut buffer) {
            Ok(bytes_read) => {
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

fn proxy_connection(mut incoming_stream: TcpStream, dst_addr: &str) -> Result<(), String> {
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
        let mut outgoing_stream = TcpStream::connect(dst_addr)
            .map_err(|e| format!("Could not establish connection to {}: {}", dst_addr, e))?;

        let mut incoming_stream_clone = incoming_stream.try_clone().map_err(|e| e.to_string())?;
        let mut outgoing_stream_clone = outgoing_stream.try_clone().map_err(|e| e.to_string())?;

        let forward = spawn(move || pipe(&mut incoming_stream, &mut outgoing_stream));
        let backward = spawn(move || pipe(&mut outgoing_stream_clone, &mut incoming_stream_clone));

        forward
            .join()
            .map_err(|e| format!("Forward failed: {:?}", e))?;
        backward
            .join()
            .map_err(|e| format!("Backward failed: {:?}", e))?;
    }

    Ok(())
}

impl NetEdit {
    fn udp_listener(&self) -> std::io::Result<()> {
        todo!();
    }

    fn tcp_listener(&self) -> Result<(), String> {
        let mut listener = TcpListener::bind(format!("127.0.0.1:{}", &self.port)).map_err(|e| {
            format!(
                "Could not establish tcp listener to 127.0.0.1:{}: {}",
                &self.port, e
            )
        })?;

        io::stdout().write_all(
            format!("Running on port {} with pid {}\n", self.port, process::id()).as_bytes(),
        );

        let dst_addr = Arc::new(format!("{}:{}", self.dst_host, self.dst_port));

        let stream: TcpStream;
        for stream in listener.incoming() {
            let stream = stream.map_err(|e| format!("Could not handle connection: {}", e))?;

            // One thread per connection
            let dst_addr = dst_addr.clone();
            spawn(move || {
                proxy_connection(stream, &dst_addr).map_err(|e| format!("{}", e));
            });
        }

        Ok(())
    }

    fn listen(&self) -> Result<(), String> {
        self.tcp_listener()
    }
}

/// netedit <protocol> <port> <dst-host:dst-port>
fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    match parse_args(args) {
        Ok(netedit) => {
            netedit.listen().unwrap();
            process::exit(0);
        }
        Err(msg) => {
            eprintln!("{}", msg);
            help();
            process::exit(1);
        }
    };
}
