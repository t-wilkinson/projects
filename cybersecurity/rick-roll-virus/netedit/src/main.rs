#![allow(unused)]
mod netedit;
mod proxy;
use crate::netedit::parse_args;
use std::env;
use std::process;

fn help() {
    println!("Usage: netedit <protocol> <port> <dst-host:dst-port>");
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

    Ok(())
}
