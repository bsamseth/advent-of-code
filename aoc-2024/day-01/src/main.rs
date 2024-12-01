use aocd::prelude::*;

#[aocd(2024, 1)]
fn main() {
    let mut left = Vec::new();
    let mut right = Vec::new();
    for line in input!().lines() {
        let mut iter = line.split_whitespace();
        left.push(iter.next().unwrap().parse::<u64>().unwrap());
        right.push(iter.next().unwrap().parse::<u64>().unwrap());
    }

    left.sort_unstable();
    right.sort_unstable();

    let sum_of_diffs = left
        .iter()
        .zip(right.iter())
        .map(|(l, r)| r.max(l) - r.min(l))
        .sum::<u64>();
    submit!(1, sum_of_diffs);

    let sim_score = left
        .iter()
        .map(|l| *l * right.iter().filter(|r| l == *r).count() as u64)
        .sum::<u64>();
    submit!(2, sim_score);
}
