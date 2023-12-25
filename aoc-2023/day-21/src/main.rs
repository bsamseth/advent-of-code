use aocd::prelude::*;

#[aocd(2023, 21)]
fn main() {
    let input = input!();
    let start = input
        .lines()
        .enumerate()
        .find_map(|(i, l)| {
            l.chars()
                .enumerate()
                .find(|(_, c)| *c == 'S')
                .map(|(j, _)| (i, j))
        })
        .unwrap();
    let grid = Grid::from(input!().as_str());

    submit!(1, tiles_reachable_in_steps(&grid, start, 64));
}

fn tiles_reachable_in_steps(grid: &Grid, start: (usize, usize), steps: usize) -> usize {
    let mut r1 = vec![vec![false; grid.0[0].len()]; grid.0.len()];
    let mut r2 = vec![vec![false; grid.0[0].len()]; grid.0.len()];

    r1[start.0][start.1] = true;

    let mut current: *mut Vec<Vec<bool>> = (&mut r1) as *mut _;
    let mut next: *mut Vec<Vec<bool>> = (&mut r2) as *mut _;

    for _ in 0..steps {
        let reachable = unsafe { &mut *current };
        let new_reachable = unsafe { &mut *next };
        zero(new_reachable);

        for i in 0..grid.0.len() {
            for j in 0..grid.0[0].len() {
                if reachable[i][j] && grid.0[i][j] {
                    if i > 0 {
                        new_reachable[i - 1][j] = true;
                    }
                    if i < grid.0.len() - 1 {
                        new_reachable[i + 1][j] = true;
                    }
                    if j > 0 {
                        new_reachable[i][j - 1] = true;
                    }
                    if j < grid.0[0].len() - 1 {
                        new_reachable[i][j + 1] = true;
                    }
                }
            }
        }

        std::mem::swap(&mut current, &mut next);
    }

    let reachable = unsafe { &*current };
    reachable
        .iter()
        .enumerate()
        .map(|(i, row)| {
            row.iter()
                .enumerate()
                .filter(|(j, b)| **b && grid.0[i][*j])
                .count()
        })
        .sum()
}

fn zero(v: &mut [Vec<bool>]) {
    for row in v {
        for b in row {
            *b = false;
        }
    }
}

type Tile = bool;
struct Grid(Vec<Vec<Tile>>);

impl From<&str> for Grid {
    fn from(s: &str) -> Self {
        Grid(
            s.lines()
                .map(|l| l.chars().map(|c| c != '#').collect())
                .collect(),
        )
    }
}
