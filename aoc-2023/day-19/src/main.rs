use aocd::prelude::*;
use itertools::Itertools;
use std::collections::HashMap;

use day19::{part1, part2, Part, Workflow};

#[aocd(2023, 19)]
fn main() {
    let mut input = input!().leak().split("\n\n");
    let workflows = input
        .next()
        .unwrap()
        .lines()
        .map(Workflow::from)
        .map(|w| (w.name, w))
        .collect::<HashMap<_, _>>();
    let parts = input.next().unwrap().lines().map(Part::from).collect_vec();

    submit!(1, part1::solve(&workflows, &parts));
    submit!(2, part2::solve(&workflows))
}
