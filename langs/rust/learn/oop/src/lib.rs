#[allow(dead_code)]

pub fn run() {
    let hello1 = Hello1 {
        msg: String::from("hello world"),
    };
    let greetings = vec![Box::new(hello1)];
    for v in greetings {
        v.hello();
    }

    let hello2 = Hello1 {
        msg: String::from("hello world"),
    };
    let x: Box<dyn Hello> = Box::new(hello2);
    x.hello();
}

struct Hello1 {
    msg: String,
}

impl Hello for Hello1 {
    fn hello(&self) {
        println!("{}", self.msg)
    }
}

pub trait Hello {
    fn hello(&self);
}

pub trait New {
    fn new() -> Self;
}
