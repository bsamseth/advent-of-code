use std::collections::BinaryHeap;

use aocd::prelude::*;

#[aocd(2023, 23)]
fn main() {
    let mut chars = input!()
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    submit!(1, solve(&chars));

    // Make all `<^v>` behave like `.`:
    for line in &mut chars {
        for c in line {
            if *c != '#' {
                *c = '.';
            }
        }
    }
    submit!(2, solve(&chars));
}

fn solve(chars: &[Vec<char>]) -> usize {
    let mut queue = BinaryHeap::new();
    queue.push(State {
        len: 0,
        y: 0,
        x: chars[0].iter().position(|x| *x == '.').unwrap(),
        path: Vec::new(),
    });
    let mut max = 0;
    while let Some(State { len, y, x, path }) = queue.pop() {
        if y == chars.len() - 1 {
            max = max.max(len);
            continue;
        }

        #[allow(
            clippy::cast_possible_wrap,
            clippy::cast_possible_truncation,
            clippy::cast_sign_loss
        )]
        for (dy, dx) in &[(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let ny = y as i32 + dy;
            let nx = x as i32 + dx;
            if ny < 0
                || nx < 0
                || ny >= chars.len() as i32
                || nx >= chars[0].len() as i32
                || chars[ny as usize][nx as usize] == '#'
                || path.contains(&(ny as usize, nx as usize))
            {
                continue;
            }

            let (ny, nx) = (ny as usize, nx as usize);
            match (chars[ny][nx], dy, dx) {
                ('.', _, _) | ('<', 0, -1) | ('>', 0, 1) | ('^', -1, 0) | ('v', 1, 0) => {
                    let mut path = path.clone();
                    path.push((ny, nx));
                    queue.push(State {
                        len: len + 1,
                        y: ny,
                        x: nx,
                        path,
                    });
                }
                _ => {}
            }
        }
    }

    max
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct State {
    len: usize,
    y: usize,
    x: usize,
    path: Vec<(usize, usize)>,
}
