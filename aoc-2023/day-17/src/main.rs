use aocd::prelude::*;
use itertools::Itertools;

const DIRECTIONS: [(isize, isize); 4] = [(0, 1), (0, -1), (1, 0), (-1, 0)];

#[aocd(2023, 17)]
fn main() {
    let grid = input!()
        .lines()
        .map(|l| l.chars().map(|c| c as u8 - b'0').collect_vec())
        .collect_vec();

    let h_cost = |(y, x): (usize, usize)| (grid.len() - 1 - y) + (grid[0].len() - 1 - x);

    let mut seen = vec![vec![0; grid[0].len()]; grid.len()];
    let mut queue = std::collections::BinaryHeap::new();
    queue.push(State {
        path: vec![(0, 0)],
        pos: (0, 0),
        prev: (0, 0),
        g_cost: 0,
        h_cost: h_cost((0, 0)),
        straights: 0,
    });

    loop {
        let state = queue.pop().unwrap();

        if state.pos == (grid.len() - 1, grid[0].len() - 1) {
            submit!(1, state.g_cost);
            break;
        }

        let flag = state.dir_flag() << (8 * state.straights);

        if seen[state.pos.0][state.pos.1] & flag != 0 {
            continue;
        }

        seen[state.pos.0][state.pos.1] |= flag;

        for (y, x) in DIRECTIONS {
            let new_pos = (state.pos.0 as isize + y, state.pos.1 as isize + x);
            if new_pos.0 < 0
                || new_pos.1 < 0
                || new_pos.0 as usize >= grid.len()
                || new_pos.1 as usize >= grid[0].len()
                || (new_pos.0 as usize, new_pos.1 as usize) == state.prev
            {
                continue;
            }

            let new_dir = (
                new_pos.0 - state.pos.0 as isize,
                new_pos.1 - state.pos.1 as isize,
            );
            let prev_dir = (
                state.pos.0 as isize - state.prev.0 as isize,
                state.pos.1 as isize - state.prev.1 as isize,
            );

            let new_straights = if new_dir == prev_dir {
                state.straights + 1
            } else {
                1
            };

            if new_straights > 3 {
                continue;
            }

            let new_state = State {
                pos: (new_pos.0 as usize, new_pos.1 as usize),
                prev: state.pos,
                g_cost: state.g_cost + grid[new_pos.0 as usize][new_pos.1 as usize] as usize,
                h_cost: h_cost((new_pos.0 as usize, new_pos.1 as usize)),
                straights: new_straights,
                path: state
                    .path
                    .clone()
                    .into_iter()
                    .chain(vec![(new_pos.0 as usize, new_pos.1 as usize)])
                    .collect_vec(),
            };

            queue.push(new_state);
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct State {
    path: Vec<(usize, usize)>,
    pos: (usize, usize),
    prev: (usize, usize),
    g_cost: usize,
    h_cost: usize,
    straights: u8,
}

impl State {
    fn f_cost(&self) -> usize {
        self.g_cost + self.h_cost
    }

    fn dir(&self) -> (isize, isize) {
        (
            self.pos.0 as isize - self.prev.0 as isize,
            self.pos.1 as isize - self.prev.1 as isize,
        )
    }

    fn dir_flag(&self) -> u64 {
        1u64 << DIRECTIONS
            .iter()
            .position(|&d| d == self.dir())
            .unwrap_or(4)
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other
            .f_cost()
            .cmp(&self.f_cost())
            .then_with(|| other.straights.cmp(&self.straights))
            .then_with(|| self.pos.cmp(&other.pos))
            .then_with(|| other.prev.cmp(&self.prev))
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn print_grid(grid: &[Vec<u8>], state: &State) {
    println!();
    for (i, row) in grid.iter().enumerate() {
        for (j, cell) in row.iter().enumerate() {
            if (i, j) == state.pos {
                print!("\x1b[0;92m{}\x1b[0m", cell);
            } else if state.path.contains(&(i, j)) {
                print!("\x1b[0;91m{}\x1b[0m", cell);
            } else {
                print!("{}", cell);
            }
        }
        println!();
    }
    println!();
}
