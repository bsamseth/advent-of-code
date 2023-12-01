use aocd::prelude::*;

#[aocd(2023, 1)]
fn main() {
    let (part1, part2) = input!()
        .lines()
        .map(|line| (part1_digits(line), part2_digits(line)))
        .map(|(p1, p2)| (calibration_number(p1), calibration_number(p2)))
        .fold((0, 0), |(p1, p2), (n1, n2)| (p1 + n1, p2 + n2));

    submit!(1, part1);
    submit!(2, part2);
}

fn calibration_number(numbers: Vec<u32>) -> u32 {
    assert!(!numbers.is_empty());
    numbers[0] * 10 + numbers[numbers.len() - 1]
}
fn part1_digits(line: &str) -> Vec<u32> {
    line.chars()
        .filter_map(|c| c.to_digit(10))
        .collect::<Vec<_>>()
}
fn part2_digits(line: &str) -> Vec<u32> {
    let digits_as_words = [
        "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    ];
    let chars = line.chars().collect::<Vec<_>>();
    let mut digits = Vec::new();
    for i in 0..chars.len() {
        if let Some(digit) = chars[i].to_digit(10) {
            digits.push(digit);
        } else if let Some(digit) = digits_as_words
            .iter()
            .position(|&w| line[i..].starts_with(w))
        {
            digits.push(digit as u32);
        }
    }
    digits
}
