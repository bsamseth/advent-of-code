use aocd::prelude::*;
use itertools::Itertools;

#[aocd(2023, 17)]
fn main() {
    let grid = input!()
        .lines()
        .map(|l| l.chars().map(|c| c as u8 - b'0').collect_vec())
        .collect_vec();

    submit!(1, solve(&grid, 1, 3));
    submit!(2, solve(&grid, 4, 10));
}

fn solve(grid: &[Vec<u8>], min_step: usize, max_step: usize) -> usize {
    let h_cost = |(y, x): (usize, usize)| (grid.len() - 1 - y) + (grid[0].len() - 1 - x);

    let mut seen: Vec<Vec<Dir>> = vec![vec![Dir(0); grid[0].len()]; grid.len()];
    let mut queue = std::collections::BinaryHeap::new();
    [Dir::updown(), Dir::leftright()].iter().for_each(|&d| {
        queue.push(State {
            pos: (0, 0),
            allowed_dir: d,
            g_cost: 0,
            h_cost: h_cost((0, 0)),
        });
    });

    loop {
        let state = queue.pop().unwrap();

        {
            let seen = &mut seen[state.pos.0][state.pos.1];
            if *seen & state.allowed_dir {
                continue;
            }
            *seen |= state.allowed_dir;
        }

        if state.pos == (grid.len() - 1, grid[0].len() - 1) {
            return state.g_cost;
        }

        let new_direction = state.allowed_dir.switch();
        for (y, x) in state.allowed_dir {
            let mut new_g_cost = state.g_cost;
            for step in 1..=max_step {
                let new_pos = (
                    state.pos.0 as isize + y * step as isize,
                    state.pos.1 as isize + x * step as isize,
                );

                if new_pos.0 < 0
                    || new_pos.1 < 0
                    || new_pos.0 as usize >= grid.len()
                    || new_pos.1 as usize >= grid[0].len()
                {
                    break;
                }

                new_g_cost += grid[new_pos.0 as usize][new_pos.1 as usize] as usize;

                if step < min_step || seen[new_pos.0 as usize][new_pos.1 as usize] & new_direction {
                    // The seen-check isn't strictly necessary, but it makes the search faster.
                    continue;
                }

                let new_state = State {
                    pos: (new_pos.0 as usize, new_pos.1 as usize),
                    g_cost: new_g_cost,
                    h_cost: h_cost((new_pos.0 as usize, new_pos.1 as usize)),
                    allowed_dir: new_direction,
                };

                queue.push(new_state);
            }
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
struct Dir(u8);

impl Dir {
    fn leftright() -> Self {
        Self(1)
    }
    fn updown() -> Self {
        Self(2)
    }
    fn switch(&self) -> Self {
        Self(((self.0 - 1) ^ 1) + 1)
    }
}

impl std::ops::BitAnd for Dir {
    type Output = bool;

    fn bitand(self, rhs: Self) -> Self::Output {
        self.0 & rhs.0 != 0
    }
}
impl std::ops::BitOrAssign for Dir {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl IntoIterator for Dir {
    type Item = (isize, isize);
    type IntoIter = std::array::IntoIter<Self::Item, 2>;

    fn into_iter(self) -> Self::IntoIter {
        [[(0, 1), (0, -1)], [(1, 0), (-1, 0)]][self.0 as usize - 1].into_iter()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct State {
    pos: (usize, usize),
    allowed_dir: Dir,
    g_cost: usize,
    h_cost: usize,
}

impl State {
    fn f_cost(&self) -> usize {
        self.g_cost + self.h_cost
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other
            .f_cost()
            .cmp(&self.f_cost())
            .then_with(|| self.pos.cmp(&other.pos))
            .then_with(|| self.allowed_dir.cmp(&other.allowed_dir))
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
