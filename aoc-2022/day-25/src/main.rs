use aocd::*;
use derive_more::{Add, Sum};

const DIGITS: [char; 5] = ['=', '-', '0', '1', '2'];

#[derive(Add, Sum)]
struct Snafu(i64);

impl From<&str> for Snafu {
    fn from(s: &str) -> Self {
        Self(
            s.chars()
                .rev()
                .enumerate()
                .map(|(i, c)| {
                    (match c {
                        '=' => -2,
                        '-' => -1,
                        '0' => 0,
                        '1' => 1,
                        _ => 2,
                    }) * 5i64.pow(i as u32)
                })
                .sum(),
        )
    }
}

impl From<Snafu> for String {
    fn from(snafu: Snafu) -> Self {
        let mut n = snafu.0;
        let mut s = String::new();
        while n > 0 {
            let c = ((n % 10) + 2) % 5;
            s.push(DIGITS[c as usize]);
            n = (n - (c - 2)) / 5;
        }
        s.chars().rev().collect()
    }
}

#[aocd(2022, 25)]
fn main() {
    let sum: Snafu = input!().lines().map(|l| l.into()).sum();
    submit!(1, String::from(sum));
}
