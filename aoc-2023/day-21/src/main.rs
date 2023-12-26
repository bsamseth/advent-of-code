use aocd::prelude::*;

#[aocd(2023, 21)]
fn main() {
    let grid = Grid::from(input!().as_str());

    let n = grid.grid.len();
    let half = (n - 1) / 2;

    let grid = grid.expand(2);
    let counts = tiles_reachable_in_steps(&grid, half + n * 2);

    submit!(1, counts[0]);

    // Construct a quadratic based on x=[0, 1, 2] and y=[counts[half], counts[half + n], counts[half + 2 * n]]
    // and evaluate it at x=target_divided. Everything happens to work out as integers.
    let target_steps = 26501365;
    let target_divided = (target_steps - half) / n;

    // Formula based on deriving the general solution for a quadratic from 3 points, and simplyfing
    // using the values of x.
    let (y1, y2, y3) = (counts[1], counts[2], counts[3]);
    let a = (y3 + y1 - 2 * y2) / 2;
    let b = (y2 - y1) - a;
    let c = y1;
    let result = a * target_divided * target_divided + b * target_divided + c;

    submit!(2, result);
}

pub fn tiles_reachable_in_steps(grid: &Grid, steps: usize) -> Vec<usize> {
    let mut counts = Vec::with_capacity(4);
    let mut r1 = vec![vec![false; grid.grid[0].len()]; grid.grid.len()];
    let mut r2 = vec![vec![false; grid.grid[0].len()]; grid.grid.len()];

    r1[grid.start.0][grid.start.1] = true;

    let mut current: *mut Vec<Vec<bool>> = (&mut r1) as *mut _;
    let mut next: *mut Vec<Vec<bool>> = (&mut r2) as *mut _;

    for step in 1..=steps {
        let reachable = unsafe { &mut *current };
        let new_reachable = unsafe { &mut *next };
        zero(new_reachable);

        for i in 0..grid.grid.len() {
            for j in 0..grid.grid[0].len() {
                if reachable[i][j] && grid.grid[i][j] {
                    new_reachable[i - 1][j] = true;
                    new_reachable[i + 1][j] = true;
                    new_reachable[i][j - 1] = true;
                    new_reachable[i][j + 1] = true;
                }
            }
        }

        if step == 64 || (step > 64 && (step - 65) % 131 == 0) {
            counts.push(grid.count(new_reachable));
        }
        std::mem::swap(&mut current, &mut next);
    }

    counts
}

type Tile = bool;
pub struct Grid {
    pub grid: Vec<Vec<Tile>>,
    start: (usize, usize),
}

impl From<&str> for Grid {
    fn from(s: &str) -> Self {
        Grid {
            grid: s
                .lines()
                .map(|l| l.chars().map(|c| c != '#').collect())
                .collect(),
            start: s
                .lines()
                .enumerate()
                .find_map(|(i, l)| {
                    l.chars()
                        .enumerate()
                        .find(|(_, c)| *c == 'S')
                        .map(|(j, _)| (i, j))
                })
                .unwrap(),
        }
    }
}

impl Grid {
    fn count(&self, reachable: &[Vec<bool>]) -> usize {
        reachable
            .iter()
            .enumerate()
            .map(|(i, row)| {
                row.iter()
                    .enumerate()
                    .filter(|(j, b)| **b && self.grid[i][*j])
                    .count()
            })
            .sum()
    }

    pub fn expand(self, by: usize) -> Self {
        let (n, m) = (self.grid.len(), self.grid[0].len());
        let x = 2 * by + 1;
        let mut new_grid = vec![vec![false; m * x]; n * x];

        for k in 0..x {
            for l in 0..x {
                for i in 0..self.grid.len() {
                    for j in 0..self.grid[0].len() {
                        new_grid[i + k * n][j + l * m] = self.grid[i][j];
                    }
                }
            }
        }

        Grid {
            grid: new_grid,
            start: (self.start.0 + by * n, self.start.1 + by * m),
        }
    }
}

impl std::fmt::Display for Grid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, row) in self.grid.iter().enumerate() {
            for (j, b) in row.iter().enumerate() {
                if (i, j) == self.start {
                    write!(f, "⭐")?;
                } else {
                    write!(f, "{}", if *b { '.' } else { '#' })?;
                }
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

fn zero(v: &mut [Vec<bool>]) {
    for row in v {
        for b in row {
            *b = false;
        }
    }
}
