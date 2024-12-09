#![allow(clippy::cast_possible_wrap, clippy::cast_sign_loss)]
use std::collections::HashSet;

use aocd::prelude::*;
use itertools::Itertools;

#[aocd(2024, 8)]
fn main() {
    let map: Vec<Vec<char>> = input!()
        .lines()
        .map(|line| line.chars().collect())
        .collect();
    let antennas = (0..map.len())
        .cartesian_product(0..map[0].len())
        .filter(|(i, j)| map[*i][*j] != '.')
        .collect_vec();
    let mut antinodes_p1 = HashSet::new();
    let mut antinodes_p2 = HashSet::new();
    for (i, j) in &antennas {
        for (k, l) in &antennas {
            if (i == k && j == l) || map[*i][*j] != map[*k][*l] {
                continue;
            }
            antinodes_p2.insert((*i, *j));

            let (i, j, k, l) = (*i as isize, *j as isize, *k as isize, *l as isize);

            let dy = i - k;
            let dx = j - l;
            let (mut ny, mut nx) = (i + dy, j + dx);
            let mut first = true;
            while ny >= 0 && ny < map.len() as isize && nx >= 0 && nx < map[0].len() as isize {
                if first {
                    first = false;
                    antinodes_p1.insert((ny as usize, nx as usize));
                }
                antinodes_p2.insert((ny as usize, nx as usize));
                ny += dy;
                nx += dx;
            }
        }
    }

    submit!(1, antinodes_p1.len());
    submit!(2, antinodes_p2.len());
}
