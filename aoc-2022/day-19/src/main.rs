use aocd::*;
use itertools::sorted;
use itertools::Itertools;
use regex::Regex;
use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct Robot {
    active: bool,
    produces: char,
    costs: Vec<(isize, char)>,
}

#[derive(Debug, Clone)]
struct Inventory {
    materials: HashMap<char, isize>,
    robots: Vec<Robot>,
}

type Blueprint = Vec<Robot>;

std::thread_local! {
  static CACHE : RefCell<HashMap<String, isize>> = RefCell::new(HashMap::new());
}

fn sign(inventory: &Inventory, time: u32) -> String {
    let mut s = String::new();
    s.push_str(&format!("t{} ", time));
    s.push_str(&format!("o{}", inventory.materials.get(&'o').unwrap_or(&0)));
    s.push_str(&format!("c{}", inventory.materials.get(&'c').unwrap_or(&0)));
    s.push_str(&format!("b{}", inventory.materials.get(&'b').unwrap_or(&0)));
    s.push_str(&format!(
        "g{} ",
        inventory.materials.get(&'g').unwrap_or(&0)
    ));
    s.push_str(&format!(
        "o{}",
        inventory
            .robots
            .iter()
            .filter(|r| r.produces == 'o')
            .count()
    ));
    s.push_str(&format!(
        "c{}",
        inventory
            .robots
            .iter()
            .filter(|r| r.produces == 'c')
            .count()
    ));
    s.push_str(&format!(
        "f{}",
        inventory
            .robots
            .iter()
            .filter(|r| r.produces == 'b')
            .count()
    ));
    s.push_str(&format!(
        "b{}",
        inventory
            .robots
            .iter()
            .filter(|r| r.produces == 'g')
            .count()
    ));
    s
}

fn simulate(blueprint: &Blueprint, inventory: &mut Inventory, time: u32) -> isize {
    if time == 6 {
        return *inventory.materials.get(&'g').unwrap_or(&0);
    } else if let Some(cached) = CACHE.with(|c| c.borrow().get(&sign(inventory, time)).cloned()) {
        return cached;
    }

    let mut best = 0;

    for purchases in blueprint.iter().powerset() {
        let mut costs = HashMap::new();
        for purchase in purchases.iter() {
            for (amount, material) in &purchase.costs {
                *costs.entry(*material).or_insert(0) += amount;
            }
        }
        if costs
            .iter()
            .any(|(material, amount)| inventory.materials.get(material).unwrap_or(&0) < amount)
        {
            continue;
        }

        let mut new_inventory = inventory.clone();
        for robot in purchases {
            for (n, c) in &robot.costs {
                new_inventory
                    .materials
                    .entry(*c)
                    .and_modify(|e| *e -= n)
                    .or_insert(0);
            }
            new_inventory.robots.push(robot.clone());
        }

        for robot in &mut new_inventory.robots {
            if robot.active {
                new_inventory
                    .materials
                    .entry(robot.produces)
                    .and_modify(|e| *e += 1)
                    .or_insert(1);
            } else {
                robot.active = true;
            }
        }
        best = best.max(simulate(blueprint, &mut new_inventory, time + 1));
    }

    if time == 5 {
        println!("{} {}", best, sign(inventory, time));
    }
    CACHE.with(|c| c.borrow_mut().insert(sign(inventory, time), best));
    best
}

#[aocd(2022, 19)]
fn main() {
    let input = "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.";

    let numbers = Regex::new(r"\d+").unwrap();
    let blueprints = input
        .lines()
        .map(|l| {
            let n = numbers
                .captures_iter(l)
                .map(|c| c[0].parse::<isize>().unwrap())
                .collect::<Vec<_>>();
            vec![
                Robot {
                    produces: 'o',
                    costs: vec![(n[1], 'o')],
                    active: false,
                },
                Robot {
                    produces: 'c',
                    costs: vec![(n[2], 'o')],
                    active: false,
                },
                Robot {
                    produces: 'b',
                    costs: vec![(n[3], 'o'), (n[4], 'c')],
                    active: false,
                },
                Robot {
                    produces: 'g',
                    costs: vec![(n[5], 'o'), (n[6], 'b')],
                    active: false,
                },
            ]
        })
        .collect::<Vec<_>>();

    for blueprint in blueprints {
        let mut inventory = Inventory {
            materials: HashMap::new(),
            robots: vec![Robot {
                produces: 'o',
                costs: vec![],
                active: true,
            }],
        };
        println!("{}", simulate(&blueprint, &mut inventory, 0));
        break;
    }
}
