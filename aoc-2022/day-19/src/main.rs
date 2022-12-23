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

fn search(blueprint: &[[u16; 4]; 4]) -> u16 {
    let max_time = 24;
    let mut queue = Vec::new();
    queue.push(State {
        elapsed: 0,
        materials: [0; 4],
        robots: [0, 0, 0, 1],
    });
    let mut seen = HashSet::new();

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
    //     let input = "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
    // Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.";

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
        .map(|blueprint| search(blueprint))
        .enumerate()
        .map(|(idx, geodes)| (idx + 1) * (geodes as usize))
        .sum();

    submit!(1, part1);
}
