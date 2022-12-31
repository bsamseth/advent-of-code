use aocd::*;

fn is_blizzard_check(map: &Vec<Vec<char>>, row: u16, col: u16, time: u16) -> bool {
    for i in 0..map.len() {
        match map[i][col as usize] {
            '^' if (i as i32 - time as i32).rem_euclid(map.len() as i32) == (row as i32) => {
                return true
            }
            'v' if (i + time as usize).rem_euclid(map.len()) == row as usize => return true,
            _ => (),
        }
    }
    for j in 0..map[0].len() {
        match map[row as usize][j] {
            '<' if (j as i32 - time as i32).rem_euclid(map[0].len() as i32) == (col as i32) => {
                return true
            }
            '>' if (j + time as usize).rem_euclid(map[0].len()) == col as usize => return true,
            _ => (),
        }
    }
    false
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct State {
    time: u16,
    row: u16,
    col: u16,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other
            .time
            .cmp(&self.time)
            .then_with(|| (self.row + self.col).cmp(&(other.row + other.col)))
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn push_no_dup(
    queue: &mut std::collections::BinaryHeap<State>,
    visited: &mut std::collections::HashSet<State>,
    state: State,
) {
    if !visited.contains(&state) {
        queue.push(state.clone());
        visited.insert(state);
    }
}

fn solve(map: &Vec<Vec<char>>, start_time: u16) -> u16 {
    let mut cache = std::collections::HashMap::new();
    let mut is_blizzard = |state: &State| {
        if let Some(&b) = cache.get(state) {
            return b;
        }
        let b = is_blizzard_check(map, state.row, state.col, state.time);
        cache.insert(state.clone(), b);
        b
    };

    let mut seen = std::collections::HashSet::new();
    let mut queue = std::collections::BinaryHeap::new();
    (start_time..)
        .map(|t| State {
            time: t,
            row: 0,
            col: 0,
        })
        .filter(|s| !is_blizzard(s))
        .take(100)
        .for_each(|s| push_no_dup(&mut queue, &mut seen, s));

    while let Some(State { time, row, col }) = queue.pop() {
        if is_blizzard(&State { time, row, col }) {
            continue;
        }

        if row == map.len() as u16 - 1 && col == map[0].len() as u16 - 1 {
            return time + 1;
        }

        if row > 0 {
            push_no_dup(
                &mut queue,
                &mut seen,
                State {
                    time: time + 1,
                    row: row - 1,
                    col,
                },
            );
        }
        if row < map.len() as u16 - 1 {
            push_no_dup(
                &mut queue,
                &mut seen,
                State {
                    time: time + 1,
                    row: row + 1,
                    col,
                },
            );
        }
        if col > 0 {
            push_no_dup(
                &mut queue,
                &mut seen,
                State {
                    time: time + 1,
                    row,
                    col: col - 1,
                },
            );
        }
        if col < map[0].len() as u16 - 1 {
            push_no_dup(
                &mut queue,
                &mut seen,
                State {
                    time: time + 1,
                    row,
                    col: col + 1,
                },
            );
        }

        push_no_dup(
            &mut queue,
            &mut seen,
            State {
                time: time + 1,
                row,
                col,
            },
        );
    }
    unreachable!()
}

fn flip(map: &[Vec<char>]) -> Vec<Vec<char>> {
    let mut new_map = map.to_owned();
    for (i, row) in map.iter().rev().enumerate() {
        for (j, c) in row.iter().rev().enumerate() {
            new_map[i][j] = match c {
                '^' => 'v',
                'v' => '^',
                '<' => '>',
                '>' => '<',
                x => *x,
            }
        }
    }
    new_map
}

#[aocd(2022, 24)]
fn main() {
    let map = input!()
        .lines()
        .map(|l| {
            let l = l.chars().collect::<Vec<_>>();
            l[1..l.len() - 1].to_vec()
        })
        .collect::<Vec<_>>();

    let map = map[1..map.len() - 1].to_vec();

    let part1 = solve(&map, 1);
    let back = solve(&flip(&map), part1 + 1);
    let part2 = solve(&map, back + 1);

    submit!(1, part1);
    submit!(2, part2);
}
