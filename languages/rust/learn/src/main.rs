#[allow(dead_code)]
use concurrency;
use minigrep;
use oop;
use std::process;
use structures;

fn main() {
    notsafe::run();
    process::exit(0);

    patterns::run();
    oop::run();
    concurrency::run();
    structures::run();
    let config = minigrep::Config {
        query: String::from("hi"),
        filename: String::from("poem.txt"),
        case_sensitive: false,
    };
    minigrep::run(config).unwrap();
}

fn generic<T: ?Sized>(t: &T) {}

mod patterns {
    use rand::{thread_rng, Rng};

    // newtype
    struct Wrapper(Vec<String>);

    pub fn run() {
        let mut stack = vec![1, 2, 3];

        while let Some(top) = stack.pop() {
            println!("{}", top);
        }

        let [_x, .., _y] = [0; 10];
        let x: u32 = thread_rng().gen_range(1, 10);
        match x {
            1 | 2 => println!("1 | 2"),
            var @ 3..=5 => println!("3..=5; var={:?}", var), // inclusive
            6 if x <= 6 => println!("if x < 10"),
            _ => println!("_"),
        }
    }
}

mod notsafe {
    static mut COUNTER: u32 = 0;

    extern "C" {
        fn abs(input: i32) -> i32;
    }

    pub fn run() {
        let mut num = 5;

        let r1 = &num as *const i32;
        let r2 = &mut num as *mut i32;

        unsafe {
            println!("{}", *r1);
            println!("{}", *r2);
            danger();
        }

        let x: i32 = 8;
        let a1: *const i32 = &x;
        unsafe {
            println!("{}", abs(*a1));
            println!("-4: {}", abs(-4));
        }
        println!("{:?}", unsafe { *a1 });
        println!("{:?}", unsafe { *a1.offset(1) });
        println!("{:?}", unsafe { *a1.add(1) });

        // let address = 0x01234usize;
        // let r = address as *mut i32;
        // let slice: &[i32] = unsafe { slice::from_raw_parts_mut(r, 10) };
        // println!("{:?}", slice);
    }

    unsafe fn danger() {}
}

#[no_mangle]
pub extern "C" fn call_from_c() {
    println!("Just called a Rust function from C!");
}
