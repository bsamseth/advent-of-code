use itertools::Itertools;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use aocd::prelude::*;

#[aocd(2023, 14)]
fn main() {
    let mut grid = Grid::from(input!()).tilt();
    submit!(1, grid.load());

    let mut seen = std::collections::HashSet::new();
    seen.insert(grid.hash());
    let mut loads = vec![(grid.load(), grid.hash())];
    loop {
        grid = grid.cycle();
        let load = grid.load();
        loads.push((load, grid.hash()));
        if seen.contains(&grid.hash()) {
            break;
        }
        seen.insert(grid.hash());
    }

    let cycle = 1 + loads
        .iter()
        .rev()
        .skip(1)
        .position(|l| l == loads.last().unwrap())
        .unwrap();

    let target = 1_000_000_000;
    let before_cycle = loads.len() - cycle;
    let cycle_loads = &loads[before_cycle..];
    let result = cycle_loads[(target - before_cycle) % cycle];

    submit!(2, result.0);
}

struct Grid(Vec<Vec<char>>);

impl Grid {
    fn hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.0.hash(&mut hasher);
        hasher.finish()
    }

    fn cycle(self) -> Self {
        self.tilt()
            .rotate()
            .tilt()
            .rotate()
            .tilt()
            .rotate()
            .tilt()
            .rotate()
    }

    fn tilt(mut self) -> Self {
        for i in 1..self.0.len() {
            for j in 0..self.0[i].len() {
                for k in (1..=i).rev() {
                    if self.0[k][j] == 'O' && self.0[k - 1][j] == '.' {
                        self.0[k][j] = '.';
                        self.0[k - 1][j] = 'O';
                    } else {
                        break;
                    }
                }
            }
        }

        self
    }

    fn rotate(mut self) -> Self {
        let grid = (0..self.0[0].len())
            .map(|j| (0..self.0.len()).rev().map(|i| self.0[i][j]).collect())
            .collect();
        self.0 = grid;
        self
    }

    fn load(&self) -> usize {
        self.0
            .iter()
            .flatten()
            .enumerate()
            .filter(|(_, c)| **c == 'O')
            .map(|(i, _)| {
                let row = i / self.0[0].len();
                self.0.len() - row
            })
            .sum()
    }
}

impl From<String> for Grid {
    fn from(s: String) -> Self {
        Self(s.lines().map(|l| l.chars().collect_vec()).collect_vec())
    }
}
