use aocd::*;

#[aocd(2017, 1)]
fn main() {
    let digits = input!();
    let solver = |offset| {
        digits
            .chars()
            .zip(digits.chars().cycle().skip(offset))
            .filter(|(a, b)| a == b)
            .map(|(a, _)| a.to_digit(10).unwrap())
            .sum::<u32>()
    };

    submit!(1, solver(1));
    submit!(2, solver(digits.len() / 2));
}
