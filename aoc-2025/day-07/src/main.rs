use std::{
    collections::{BTreeSet, HashMap},
    convert::Infallible,
    fmt::Display,
    str::FromStr,
};

use aocd::prelude::*;

#[aocd(2025, 7)]
fn main() {
    let grid: Grid = input!().parse().unwrap();
    print!("{grid}");
    let start = grid.start();
    submit!(1, beam_splitter_count(&grid, start));
    submit!(2, many_worlds_count(&grid, start, &mut HashMap::new()));
}

fn beam_splitter_count(grid: &Grid, coord: Coordinate) -> u64 {
    let mut queue = BTreeSet::new();

    queue.insert(coord);
    let mut count = 0;
    while let Some(coord) = queue.pop_first() {
        let Some(cell) = grid.get(coord) else {
            continue;
        };

        if cell == Cell::Split {
            count += 1;
            queue.insert(coord.left());
            queue.insert(coord.right());
        } else {
            queue.insert(coord.down());
        }
    }

    count
}

fn many_worlds_count(grid: &Grid, coord: Coordinate, memory: &mut HashMap<Coordinate, u64>) -> u64 {
    if let Some(n) = memory.get(&coord) {
        *n
    } else {
        let Some(cell) = grid.get(coord) else {
            return 1;
        };
        let n = match cell {
            Cell::Start | Cell::Empty => many_worlds_count(grid, coord.down(), memory),
            Cell::Split => {
                let left = many_worlds_count(grid, coord.left(), memory);
                let right = many_worlds_count(grid, coord.right(), memory);
                left + right
            }
        };
        memory.insert(coord, n);
        n
    }
}

#[derive(Debug)]
struct Grid(Vec<Vec<Cell>>);

impl Grid {
    fn start(&self) -> Coordinate {
        let column = self.0[0].iter().position(|c| *c == Cell::Start).unwrap();
        Coordinate { row: 0, column }
    }
    fn get(&self, coord: Coordinate) -> Option<Cell> {
        self.0.get(coord.row)?.get(coord.column).copied()
    }
}

impl FromStr for Grid {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut grid = Vec::new();
        for line in s.split('\n') {
            let mut row = Vec::new();
            for c in line.as_bytes() {
                row.push(match *c {
                    b'S' => Cell::Start,
                    b'.' => Cell::Empty,
                    b'^' => Cell::Split,
                    _ => unreachable!(),
                });
            }
            grid.push(row);
        }
        Ok(Self(grid))
    }
}

impl Display for Grid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in &self.0 {
            for c in line {
                write!(f, "{}", char::from(*c as u8))?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
enum Cell {
    Start = b'S',
    Empty = b'.',
    Split = b'^',
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Coordinate {
    row: usize,
    column: usize,
}

impl Coordinate {
    fn down(&self) -> Self {
        Self {
            row: self.row + 1,
            column: self.column,
        }
    }

    fn left(&self) -> Self {
        Self {
            row: self.row,
            column: self.column.wrapping_sub(1),
        }
    }
    fn right(&self) -> Self {
        Self {
            row: self.row,
            column: self.column.wrapping_add(1),
        }
    }
}
