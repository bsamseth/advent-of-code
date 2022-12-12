mod cache;
mod client;

pub use client::Aocd;

const AOC_URL: &str = "https://adventofcode.com";

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_input() {
        let aocd = Aocd::new();
        let input = aocd.get_input(2022, 11);
        assert!(input.starts_with("Monkey 0:"));
    }

    // #[test]
    // fn test_submit() {
    //     let aocd = Aocd::new();
    //     aocd.submit(2022, 11, 1, 111210);
    // }
}
