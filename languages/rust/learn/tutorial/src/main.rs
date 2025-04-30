#![allow(dead_code)]
mod lib;
mod main_v2;
mod tests;
use crate::lib::mytest;

use rand::{thread_rng, Rng};
use std::cmp::Ordering;
use std::fs;
use std::fs::File;
use std::io;
// use std::io::Stdout;

#[macro_export]
macro_rules! vec1 {
    ( $( $x:expr ),* ) => {
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push($x);
            )*
            temp_vec
        }
    };
}

struct Animal {
    name: String,
}

#[derive(Debug)]
struct Color(u8, u8, u8);

impl Animal {
    fn first_name(&mut self) -> &str {
        &self.name[..]
    }
}

enum _IpAddrKind<'a> {
    V4,
    V6,
    Message { x: i32, y: i32 },
    Addr(&'a str),
}

impl<'a> _IpAddrKind<'a> {
    fn _call(&self) {
        let res = match self {
            _IpAddrKind::V4 => String::from("V4"),
            _IpAddrKind::V6 => String::from("V6"),
            _IpAddrKind::Message { x, y } => (x + y).to_string(),
            &_IpAddrKind::Addr(st) => String::from(st),
        };
        println!("{}", res);
    }
}

trait Test<T> {
    fn run(&self) -> &T;
}

fn main() {
    println!("Hello, world!");
    mytest::say_hello();
    lib::mytest::say_hello();
    play();

    let _x = vec1!["hi", "bye"];

    let f1 = File::open("test");
    let f1 = match f1 {
        Ok(file) => file,
        Err(err) => panic!("Problem with {:?}", err),
    };
    println!("{:?}", f1);

    let f2 = fs::read_to_string("hello.txt").expect("File does not exist");
    println!("{:?}", f2);

    let [_x, _y, _z, ..] = [0o77, 0xff, 0b10_101, 9_222];
    let my_value = Some(3_u32);
    if let Some(3) = my_value {
        println!("hi");
    }

    let mut animal1 = Animal {
        name: String::from("bob"),
    };

    let animal2 = Animal {
        name: String::from("will"),
        ..animal1
    };

    let color1 = Color(255, 255, 255);

    println!("{} vs. {} {:?}", animal1.first_name(), animal2.name, color1);
}

fn play() -> String {
    let _x: &&&&&u8;
    _x = &&&&&8;

    let x: [u8; 3] = [1, 2, 3];
    let _y = &x[1..3];

    let _y = {
        let x = 0;
        x + 1
    };

    let tup = ('a', 'b', 'c', 'd');
    let ones = [1; 5];
    println!("{}, {}, {}", tup.1, x[1], ones[0]);

    let x = String::from("hil");
    x
}

fn guess() {
    let secret_number = thread_rng().gen_range(1..5);

    loop {
        print!("? ");
        // io::stdout().flush();
        let mut guess = String::new();

        io::stdin()
            .read_line(&mut guess)
            .expect("failed to read line");

        let guess: u32 = match guess.trim().parse() {
            Ok(num) => num,
            Err(_) => {
                println!("Please input number");
                continue;
            }
        };

        match guess.cmp(&secret_number) {
            Ordering::Less => println!("Too Small!"),
            Ordering::Greater => println!("Too Great!"),
            Ordering::Equal => {
                println!("Too Equal!");
                break;
            }
        }
    }
}
