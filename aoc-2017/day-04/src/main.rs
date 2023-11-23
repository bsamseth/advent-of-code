use aocd::prelude::*;
use itertools::Itertools;

#[aocd(2017, 4)]
fn main() {
    let input = input!();

    let part1 = input
        .lines()
        .filter(|line| {
            line.split_whitespace()
                .map(String::from)
                .collect::<Vec<_>>()
                .no_dups()
        })
        .count();

    submit!(1, part1);

    let part2 = input
        .lines()
        .filter(|line| {
            line.split_whitespace()
                .map(|word| word.chars().sorted().collect::<String>())
                .collect::<Vec<_>>()
                .no_dups()
        })
        .count();

    submit!(2, part2);
}

trait NoDups {
    fn no_dups(&mut self) -> bool;
}

impl NoDups for Vec<String> {
    fn no_dups(&mut self) -> bool {
        self.sort_unstable();
        self.windows(2).all(|w| w[0] != w[1])
    }
}
