use std::collections::{HashMap, HashSet};

use aocd::prelude::*;
use itertools::iproduct;

#[aocd(2023, 22)]
fn main() {
    let mut bricks = input!().lines().map(Brick::from).collect::<Vec<_>>();

    // Sort bricks by lowest z value.
    bricks.sort_unstable_by(|a, b| {
        let a = a.0.z.min(a.1.z);
        let b = b.0.z.min(b.1.z);
        a.cmp(&b)
    });

    // Let the bricks fall one z level at a time, but only if no other brick is in the way below
    // it. The lowest possible z level is 1.

    let mut map: HashMap<(u64, u64, u64), usize> = HashMap::new();
    for (i, brick) in bricks.iter().enumerate() {
        for space in iproduct!(
            brick.0.x.min(brick.1.x)..=brick.0.x.max(brick.1.x),
            brick.0.y.min(brick.1.y)..=brick.0.y.max(brick.1.y),
            brick.0.z.min(brick.1.z)..=brick.0.z.max(brick.1.z)
        ) {
            map.insert(space, i);
        }
    }

    let mut settled_map = map.clone();

    for (i, brick) in bricks.iter().enumerate() {
        for z in (1..brick.0.z.min(brick.1.z)).rev() {
            assert!(z < brick.0.z.min(brick.1.z));
            if iproduct!(
                brick.0.x.min(brick.1.x)..=brick.0.x.max(brick.1.x),
                brick.0.y.min(brick.1.y)..=brick.0.y.max(brick.1.y),
            )
            .any(|(x, y)| settled_map.get(&(x, y, z)).is_some_and(|v| *v != i))
            {
                break;
            }

            settled_map = settled_map
                .iter()
                .map(|((x, y, z), v)| {
                    if *v == i {
                        ((*x, *y, *z - 1), *v)
                    } else {
                        ((*x, *y, *z), *v)
                    }
                })
                .collect();
        }
    }

    let supported_by: HashMap<usize, HashSet<usize>> = (0..bricks.len())
        .map(|i| {
            let mut supporters = settled_map
                .iter()
                .filter(|(_, v)| **v == i)
                .filter_map(|((x, y, z), _)| settled_map.get(&(*x, *y, *z - 1)).copied())
                .filter(|v| *v != i)
                .collect::<HashSet<_>>();
            if supporters.is_empty() {
                supporters.insert(usize::MAX);
            }
            (i, supporters)
        })
        .collect();

    let mut cant_be_removed = HashSet::new();
    for supporters in supported_by.values() {
        if supporters.len() == 1 && *supporters.iter().next().unwrap() != usize::MAX {
            cant_be_removed.insert(*supporters.iter().next().unwrap());
        }
    }

    submit!(1, bricks.len() - cant_be_removed.len());

    submit!(
        2,
        (0..bricks.len())
            .map(|i| collapse(i, supported_by.clone()))
            .sum::<usize>()
    );
}

fn collapse(brick: usize, mut supported_by: HashMap<usize, HashSet<usize>>) -> usize {
    let mut collapsed = HashSet::new();
    let mut stack = vec![brick];
    while let Some(brick) = stack.pop() {
        if collapsed.insert(brick) {
            for (idx, supporters) in &mut supported_by {
                supporters.remove(&brick);
                if supporters.is_empty() {
                    stack.push(*idx);
                }
            }
        }
    }
    collapsed.len() - 1
}

#[allow(dead_code)]
fn print_map_x(map: &HashMap<(u64, u64, u64), usize>) {
    let mut min_x = u64::MAX;
    let mut max_x = 0;
    let mut min_y = u64::MAX;
    let mut max_y = 0;
    let mut min_z = u64::MAX;
    let mut max_z = 0;

    for (x, y, z) in map.keys() {
        min_x = min_x.min(*x);
        max_x = max_x.max(*x);
        min_y = min_y.min(*y);
        max_y = max_y.max(*y);
        min_z = min_z.min(*z);
        max_z = max_z.max(*z);
    }

    for z in (0..=max_z).rev() {
        for x in min_x..=max_x {
            let c = (min_y..=max_y)
                .find_map(|y| map.get(&(x, y, z)).map(|v| format!("{v}")))
                .unwrap_or_else(|| '.'.to_string());
            if z != 0 {
                print!("{c}");
            } else {
                print!("-");
            }
        }
        println!("  <- z = {z}");
    }
}

#[allow(dead_code)]
fn print_map_y(map: &HashMap<(u64, u64, u64), usize>) {
    let mut min_x = u64::MAX;
    let mut max_x = 0;
    let mut min_y = u64::MAX;
    let mut max_y = 0;
    let mut min_z = u64::MAX;
    let mut max_z = 0;

    for (x, y, z) in map.keys() {
        min_x = min_x.min(*x);
        max_x = max_x.max(*x);
        min_y = min_y.min(*y);
        max_y = max_y.max(*y);
        min_z = min_z.min(*z);
        max_z = max_z.max(*z);
    }

    for z in (0..=max_z).rev() {
        for y in min_y..=max_y {
            let c = (min_x..=max_x)
                .find_map(|x| map.get(&(x, y, z)).map(|v| format!("{v}")))
                .unwrap_or_else(|| '.'.to_string());
            if z != 0 {
                print!("{c}");
            } else {
                print!("-");
            }
        }
        println!("  <- z = {z}");
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Point {
    x: u64,
    y: u64,
    z: u64,
}
#[derive(Debug, Clone, Eq, PartialEq)]
struct Brick(Point, Point);

impl From<&str> for Brick {
    fn from(s: &str) -> Self {
        let (left, right) = s.split_once('~').unwrap();
        Self(left.into(), right.into())
    }
}

impl From<&str> for Point {
    fn from(s: &str) -> Self {
        let mut iter = s.split(',').map(|s| s.parse::<u64>().unwrap());
        let x = iter.next().unwrap();
        let y = iter.next().unwrap();
        let z = iter.next().unwrap();
        Self { x, y, z }
    }
}
