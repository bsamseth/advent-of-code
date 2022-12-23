use rayon::prelude::*;
use std::collections::HashSet;

use aocd::*;
use regex::Regex;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct State {
    elapsed: u16,
    materials: [u16; 4],
    robots: [u16; 4],
}

fn search(blueprint: &[[u16; 4]; 4], max_time: u16) -> u16 {
    let mut queue = Vec::new();
    queue.push(State {
        elapsed: 0,
        materials: [0; 4],
        robots: [0, 0, 0, 1],
    });
    let mut seen = HashSet::new();

    let mut max_robots = [u16::MAX; 4];
    for i in 1..4 {
        max_robots[i] = blueprint.iter().map(|cost| cost[i]).max().unwrap();
    }

    let mut best = 0;
    while let Some(State {
        materials,
        robots,
        elapsed,
    }) = queue.pop()
    {
        // Try to build each robot by waiting the required time for materials to be available (or skip if it can't be done in time).
        // If the robot is built, add a new state to the queue.
        for (i, costs) in blueprint.iter().rev().enumerate() {
            if robots[i] == max_robots[i] {
                continue;
            }
            let need_to_wait = (1..4)
                .map(|idx| {
                    match costs[idx] {
                        cost if cost <= materials[idx] => 0,
                        // no target bot type made yet
                        // we can't build it (it takes more than max_time to build it).
                        _ if robots[idx] == 0 => max_time + 1,
                        _ => (costs[idx] - materials[idx] + robots[idx] - 1) / robots[idx],
                    }
                })
                .max()
                .unwrap();

            if elapsed + need_to_wait + 1 >= max_time {
                continue;
            }

            let mut new_materials = materials;
            for idx in 0..4 {
                new_materials[idx] += (need_to_wait + 1) * robots[idx] - costs[idx];
            }

            let mut new_robots = robots;
            new_robots[i] += 1;

            // Alpha pruning: If we built geode robots every turn from here on out and we still wouldn't beat the current best,
            // then we can stop searching this branch. geode robots * remianing time + current geodes + sum(i for i in 0..remaining_time)
            let remaining_time = max_time - elapsed - need_to_wait - 1;
            if ((remaining_time - 1) * remaining_time) / 2
                + new_materials[0]
                + remaining_time * new_robots[0]
                <= best
            {
                continue;
            }

            let new_state = State {
                elapsed: elapsed + need_to_wait + 1,
                materials: new_materials,
                robots: new_robots,
            };
            if seen.insert(new_state) {
                queue.push(new_state);
            }
        }

        // Finally, compute how many geodes we'd get by letting what we have now run uninterrupted for the rest of the time.
        best = best.max(materials[0] + robots[0] * (max_time - elapsed));
    }
    best
}

#[aocd(2022, 19)]
fn main() {
    let numbers = Regex::new(r"\d+").unwrap();
    let blueprints = input!()
        .lines()
        .map(|l| {
            let n = numbers
                .captures_iter(l)
                .map(|c| c[0].parse::<u16>().unwrap())
                .collect::<Vec<_>>();
            [
                [0, 0, 0, n[1]],
                [0, 0, 0, n[2]],
                [0, 0, n[4], n[3]],
                [0, n[6], 0, n[5]],
            ]
        })
        .collect::<Vec<_>>();

    let part1: usize = blueprints
        .par_iter()
        .map(|blueprint| search(blueprint, 24))
        .enumerate()
        .map(|(idx, geodes)| (idx + 1) * (geodes as usize))
        .sum();

    submit!(1, part1);

    let part2: usize = blueprints[0..3]
        .par_iter()
        .map(|blueprint| search(blueprint, 32))
        .collect::<Vec<_>>()
        .iter()
        .fold(1usize, |a, b| a * usize::from(*b));
    submit!(2, part2);
}
