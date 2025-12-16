use std::{convert::Infallible, str::FromStr};

use aocd::prelude::*;
use itertools::Itertools;

#[aocd(2025, 4)]
fn main() {
    let mut grid: Grid = input!().parse().unwrap();

    let mut count = 0;
    for i in 0..grid.height {
        for j in 0..grid.width {
            if matches!(grid.get(i, j), Some(Cell::PaperRoll)) && grid.number_of_neighbors(i, j) < 4
            {
                count += 1;
            }
        }
    }
    submit!(1, count);

    let mut count = 0;
    loop {
        let mut found_one = false;
        for i in 0..grid.height {
            for j in 0..grid.width {
                if matches!(grid.get(i, j), Some(Cell::PaperRoll))
                    && grid.number_of_neighbors(i, j) < 4
                {
                    grid.remove(i, j);
                    count += 1;
                    found_one = true;
                }
            }
        }

        if !found_one {
            break;
        }
    }
    submit!(2, count);
}

struct Grid {
    data: Box<[Cell]>,
    pub height: usize,
    pub width: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Cell {
    Empty,
    PaperRoll,
}

impl Grid {
    fn get(&self, row: usize, column: usize) -> Option<Cell> {
        if row >= self.height || column >= self.width {
            return None;
        }
        self.data.get(row * self.height + column).copied()
    }

    fn remove(&mut self, row: usize, column: usize) {
        self.data[row * self.height + column] = Cell::Empty;
    }

    #[expect(clippy::cast_sign_loss, clippy::cast_possible_wrap)]
    fn number_of_neighbors(&self, row: usize, column: usize) -> usize {
        let row = row as isize;
        let column = column as isize;
        [-1isize, 0, 1]
            .into_iter()
            .cartesian_product([-1isize, 0, 1])
            .filter(|(di, dj)| !(*di == 0 && *dj == 0))
            .map(|(di, dj)| (row.wrapping_add(di), column.wrapping_add(dj)))
            .filter_map(|(i, j)| {
                if i < 0 || j < 0 {
                    return None;
                }
                self.get(i as usize, j as usize)
            })
            .filter(|x| *x == Cell::PaperRoll)
            .count()
    }
}

impl FromStr for Grid {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let input: Vec<Vec<Cell>> = s
            .split('\n')
            .map(|line| {
                line.as_bytes()
                    .iter()
                    .map(|b| {
                        if *b == b'@' {
                            Cell::PaperRoll
                        } else {
                            Cell::Empty
                        }
                    })
                    .collect()
            })
            .collect();

        let height = input.len();
        let width = input[0].len();
        let data = input
            .iter()
            .flatten()
            .copied()
            .collect::<Vec<_>>()
            .into_boxed_slice();
        Ok(Self {
            data,
            height,
            width,
        })
    }
}
