mod lib;
use crate::lib::run;
use lib::Config;

use std::env;
use std::process;

/// Will run the program
/// # Example
/// ```
/// let x = 3;
/// println!("{}", x);
/// >>> 3
/// ```
fn main() {
    let config = Config::new(env::args()).unwrap_or_else(|err| {
        eprintln!("Problem parsing arguments: {}", err);
        process::exit(1);
    });

    let x = [4 - 1..12];
    println!("{:?}", x);

    if let Err(e) = run(config) {
        eprintln!("Application error: {}", e);
        process::exit(1);
    }
}
