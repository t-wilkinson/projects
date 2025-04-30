#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    #[should_panic]
    #[ignore]
    fn it_fails() {
        panic!("Error! {}", "o");
    }
}
