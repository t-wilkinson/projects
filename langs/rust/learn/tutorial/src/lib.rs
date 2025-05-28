#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn result() -> Result<(), String> {
        if 1 == 1 {
            Ok(())
        } else {
            Err(String::from("h"))
        }
    }
}

// Docs
pub mod mytest {
    use super::pointers;

    pub fn say_hello() {
        println!("hi!");
        pointers::run();
    }
}

pub mod pointers {
    #[derive(Debug)]
    enum List<T> {
        Cons(T, Box<List<T>>),
        Nil,
    }

    use self::List::{Cons, Nil};

    pub fn run() {
        let list = Cons(5, Box::new(Nil));
        println!("{:?}", list);
    }
}
