use aocd::*;

#[aocd(2022, 1)]
fn main() {
    let mut elves: Vec<_> = input!()
        .split("\n\n")
        .map(|e| e.lines().map(|l| l.parse::<u32>().unwrap()).sum())
        .collect();
    elves.sort();

    submit!(1, elves.last().unwrap());
    submit!(2, elves.iter().rev().take(3).sum::<u32>());
}
