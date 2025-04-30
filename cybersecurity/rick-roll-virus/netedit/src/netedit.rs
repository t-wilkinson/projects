use crate::proxy;
use std::fs::File;
use std::io::{self, Error, ErrorKind, Read, Write};
use std::net::{Shutdown, TcpListener, TcpStream};
use std::process;
use std::sync::Arc;
use std::thread::{spawn, JoinHandle};

pub enum Protocol {
    TCP,
    UDP,
}

pub struct NetEdit {
    pub protocol: Protocol,
    pub port: u16,
    pub dst_host: String,
    pub dst_port: String,
    pub handler: Option<fn(TcpStream, &str) -> Result<(), String>>,
}

pub fn parse_args(args: Vec<String>) -> Result<NetEdit, String> {
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
        Err(err) => return Err("Port must be a number".to_owned()),
    };

    let destination = &args[3];
    let destination: Vec<&str> = destination.split(':').collect();
    if destination.len() != 2 {
        return Err("Destination address must be in the form <host:port>".to_owned());
    };

    return Ok(NetEdit {
        protocol,
        port,
        dst_host: String::from(destination[0]),
        dst_port: String::from(destination[1]),
        handler: None,
    });
}

pub fn proxy_connection(mut incoming_stream: TcpStream, dst_addr: &str) -> Result<(), String> {
    let mut outgoing_stream = TcpStream::connect(dst_addr)
        .map_err(|e| format!("Could not establish connection to {}: {}", dst_addr, e))?;

    let mut incoming_stream_clone = incoming_stream.try_clone().map_err(|e| e.to_string())?;
    let mut outgoing_stream_clone = outgoing_stream.try_clone().map_err(|e| e.to_string())?;

    let forward = spawn(move || proxy::pipe(&mut incoming_stream, &mut outgoing_stream));
    let backward =
        spawn(move || proxy::pipe(&mut outgoing_stream_clone, &mut incoming_stream_clone));

    forward
        .join()
        .map_err(|e| format!("Forward failed: {:?}", e))?;
    backward
        .join()
        .map_err(|e| format!("Backward failed: {:?}", e))?;

    Ok(())
}

impl NetEdit {
    pub fn udp_listener(&self) -> std::io::Result<()> {
        todo!();
    }

    pub fn tcp_listener(&self) -> Result<(), String> {
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
            let mut stream = stream.map_err(|e| format!("Could not handle connection: {}", e))?;

            // One thread per connection
            let dst_addr = dst_addr.clone();
            match self.handler {
                Some(handler) => spawn(move || handler(stream, &dst_addr)),
                None => {
                    spawn(move || proxy_connection(stream, &dst_addr).map_err(|e| format!("{}", e)))
                }
            };
        }

        Ok(())
    }

    pub fn listen(&self) -> Result<(), String> {
        self.tcp_listener()
    }
}
