use aocd::prelude::*;
use itertools::Itertools;

#[aocd(2023, 11)]
fn main() {
    let grid = input!()
        .lines()
        .map(|l| l.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    submit!(1, calculate(&grid, 2));
    submit!(2, calculate(&grid, 1000000));
}

fn calculate(grid: &[Vec<char>], expansion_rate: i64) -> i64 {
    let weights = (0..grid.len())
        .map(|i| {
            (0..grid[i].len())
                .map(|j| {
                    if grid[i].iter().all(|&c| c == '.') || grid.iter().all(|row| row[j] == '.') {
                        expansion_rate
                    } else {
                        1
                    }
                })
                .collect_vec()
        })
        .collect_vec();

    grid.iter()
        .enumerate()
        .flat_map(|(i, row)| {
            row.iter()
                .enumerate()
                .filter(|(_, &c)| c != '.')
                .map(move |(j, _)| (i as i64, j as i64))
        })
        .combinations(2)
        .map(|v| {
            let a = v[0];
            let b = v[1];

            let vertical = (a.0.min(b.0)..a.0.max(b.0))
                .map(|i| weights[i as usize][0])
                .sum::<i64>();
            let horizontal = (a.1.min(b.1)..a.1.max(b.1))
                .map(|j| weights[0][j as usize])
                .sum::<i64>();

            vertical + horizontal
        })
        .sum()
}
