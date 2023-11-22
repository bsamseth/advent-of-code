use aocd::prelude::*;

#[aocd(2017, 2)]
fn main() {
    let input = input!();
    let (part1, part2) = input
        .lines()
        .map(|line| {
            let mut numbers = line
                .split_whitespace()
                .map(str::parse::<u32>)
                .collect::<Result<Vec<u32>, _>>()
                .unwrap();

            numbers.sort_unstable();

            (
                numbers[numbers.len() - 1] - numbers[0],
                numbers
                    .iter()
                    .enumerate()
                    .filter_map(|(i, &n)| {
                        numbers
                            .iter()
                            .skip(i + 1)
                            .find(|&m| m % n == 0)
                            .map(|&m| m / n)
                    })
                    .next()
                    .unwrap(),
            )
        })
        .fold((0, 0), |(sum_p1, sum_p2), (p1, p2)| {
            (sum_p1 + p1, sum_p2 + p2)
        });

    submit!(1, part1);
    submit!(2, part2);
}
