#[allow(dead_code)]
use std::time::Duration;

pub fn run() {
    // message_passing();
    // mucus();
    shared_state();
    // threads();
}

fn threads() {
    use std::thread;

    let range = 1..10;
    let handle = thread::spawn(|| {
        for i in range {
            println!("hi {}", i);
            thread::sleep(Duration::from_millis(1));
        }
    });

    for i in 1..5 {
        println!("hi current {}", i);
        thread::sleep(Duration::from_millis(1));
    }

    handle.join().unwrap();
}

fn message_passing() {
    use std::sync::mpsc;
    use std::thread;

    let (tx, rx) = mpsc::channel();
    let tx1 = tx.clone();
    let tx2 = tx.clone();

    thread::spawn(move || tx.send("a").unwrap());
    thread::spawn(move || tx1.send("b").unwrap());
    thread::spawn(move || tx2.send("c").unwrap());

    let x = rx.recv().unwrap();
    println!("value of first: {}", x);

    for v in rx {
        println!("Value received {:?}", v);
    }
}

fn mucus() {
    use std::sync::Mutex;

    let m = Mutex::new(5);

    {
        let mut num = m.lock().unwrap();
        *num = 6;
    }

    println!("m = {:?}", m);
}

fn shared_state() {
    use std::sync::{Arc, Mutex};
    use std::thread;

    let counter = Arc::new(Mutex::new(0));
    let mut handles = vec![];

    for _ in 0..10 {
        let counter = Arc::clone(&counter);
        let handle = thread::spawn(move || {
            let mut num = counter.lock().unwrap();

            *num += 1;
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    println!("Result: {}", *counter.lock().unwrap());
}

#[cfg(test)]
mod tests {}
