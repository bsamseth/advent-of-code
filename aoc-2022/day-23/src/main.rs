use std::collections::{HashMap, HashSet};

use aocd::*;

type Point = num::complex::Complex<i32>;

fn bounds(grid: &HashSet<Point>) -> (Point, Point) {
    let mut min = Point::new(i32::MAX, i32::MAX);
    let mut max = Point::new(i32::MIN, i32::MIN);
    for &p in grid {
        min = Point::new(min.re.min(p.re), min.im.min(p.im));
        max = Point::new(max.re.max(p.re), max.im.max(p.im));
    }
    (min, max)
}

fn empty_contained_squares(grid: &HashSet<Point>) -> usize {
    let (min, max) = bounds(grid);
    let mut count = 0;
    for y in min.im..=max.im {
        for x in min.re..=max.re {
            if !grid.contains(&Point::new(x, y)) {
                count += 1;
            }
        }
    }
    count
}

#[aocd(2022, 23)]
fn main() {
    let mut grid = input!()
        .lines()
        .enumerate()
        .flat_map(|(i, line)| {
            line.chars()
                .enumerate()
                .filter_map(move |(j, c)| match c {
                    '#' => Some(Point::new(j as i32, 0 - i as i32)),
                    _ => None,
                })
                .collect::<Vec<_>>()
        })
        .collect::<HashSet<_>>();

    let direction_sets = [
        vec![Point::new(1, 0), Point::new(1, 1), Point::new(1, -1)],
        vec![Point::new(0, 1), Point::new(1, 1), Point::new(-1, 1)],
        vec![Point::new(0, -1), Point::new(-1, -1), Point::new(1, -1)],
        vec![Point::new(-1, 0), Point::new(-1, 1), Point::new(-1, -1)],
    ];
    let mut dir_idx = 0;

    for round in 0.. {
        dir_idx += 1;
        let directions = direction_sets
            .iter()
            .cycle()
            .skip(dir_idx % 4)
            .take(4)
            .collect::<Vec<_>>();

        let mut proposals = HashMap::new();
        for point in grid.iter().filter(|&p| {
            directions
                .iter()
                .any(|dirs| dirs.iter().any(|&d| grid.contains(&(p + d))))
        }) {
            for dirs in directions.iter() {
                if !dirs.iter().any(|d| grid.contains(&(point + d))) {
                    proposals
                        .entry(point + dirs[0])
                        .and_modify(|(_, b)| *b = false)
                        .or_insert((*point, true));
                    break;
                }
            }
        }

        let mut c = 0;
        for (to, (from, _)) in proposals.iter().filter(|(_, (_, b))| *b) {
            grid.insert(*to);
            grid.remove(from);
            c += 1;
        }

        if c == 0 {
            submit!(2, round + 1);
            break;
        }

        if round == 10 {
            submit!(1, empty_contained_squares(&grid))
        }
    }
}
