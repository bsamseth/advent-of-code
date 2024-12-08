#![allow(clippy::cast_possible_wrap, clippy::cast_sign_loss)]
use std::collections::HashSet;

use aocd::prelude::*;
use itertools::Itertools;

#[aocd(2024, 6)]
fn main() {
    let mut map = input!()
        .lines()
        .map(|line| line.chars().collect_vec())
        .collect_vec();

    let pos = (0..map.len() as isize)
        .cartesian_product(0..(map[0].len() as isize))
        .find(|p| map[p.0 as usize][p.1 as usize] == '^')
        .unwrap();
    let dir = (-1, 0);

    let path = run(&map, pos, dir)
        .unwrap()
        .into_iter()
        .map(|(p, _)| p)
        .collect::<HashSet<_>>();
    submit!(1, path.len());

    let mut p2 = 0;
    for obs_pos in path {
        let prev = map[obs_pos.0 as usize][obs_pos.1 as usize];
        map[obs_pos.0 as usize][obs_pos.1 as usize] = '#';
        let res = run(&map, pos, dir);
        map[obs_pos.0 as usize][obs_pos.1 as usize] = prev;
        if res.is_none() {
            p2 += 1;
        }
    }
    submit!(2, p2);
}

type Path = HashSet<((isize, isize), (isize, isize))>;

fn run(map: &[Vec<char>], mut pos: (isize, isize), mut dir: (isize, isize)) -> Option<Path> {
    let get = |(y, x): (isize, isize)| {
        if y < 0 || x < 0 || y >= map.len() as isize || x >= map[0].len() as isize {
            None
        } else {
            Some(map[y as usize][x as usize])
        }
    };

    let mut seen = HashSet::new();
    loop {
        if !seen.insert((pos, dir)) {
            return None;
        }
        let new = (pos.0 + dir.0, pos.1 + dir.1);
        match get(new) {
            Some('.' | '^') => pos = new,
            Some('#') => dir = (dir.1, -dir.0),
            None => break,
            _ => unreachable!(),
        }
    }
    Some(seen)
}
