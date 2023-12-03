use aocd::prelude::*;
use std::collections::HashMap;

#[aocd(2023, 3)]
fn main() {
    let grid = input!()
        .lines()
        .map(|l| l.chars().collect::<Vec<_>>())
        .collect::<Vec<Vec<char>>>();

    let mut gears = HashMap::new();

    let mut part1 = 0;
    let mut i = 0;
    while i < grid.len() {
        let mut j = 0;
        while j < grid[i].len() {
            if grid[i][j].is_ascii_digit() {
                let start = j;
                let mut end = j;
                while let Some(n) = grid[i].get(end + 1) {
                    if !n.is_ascii_digit() {
                        break;
                    }
                    end += 1;
                }

                let num = grid[i][start..=end]
                    .iter()
                    .collect::<String>()
                    .parse::<usize>()
                    .unwrap();

                if (i.saturating_sub(1)..=(i + 1)).any(|k| {
                    (start.saturating_sub(1)..=(end + 1))
                        .filter_map(|l| grid.get(k).and_then(|r| r.get(l)))
                        .any(|c| !c.is_ascii_digit() && *c != '.')
                }) {
                    part1 += num;
                }

                (i.saturating_sub(1)..=(i + 1)).for_each(|k| {
                    (start.saturating_sub(1)..=(end + 1))
                        .filter(|&l| *grid.get(k).and_then(|r| r.get(l)).unwrap_or(&'.') == '*')
                        .for_each(|l| {
                            if let Some((count, n)) = gears.get_mut(&(k, l)) {
                                *count += 1;
                                *n *= num;
                            } else {
                                gears.insert((k, l), (1, num));
                            }
                        })
                });

                j = end;
            }
            j += 1;
        }
        i += 1;
    }

    submit!(1, part1);
    submit!(
        2,
        gears
            .values()
            .filter(|(count, _)| *count == 2)
            .map(|(_, n)| *n)
            .sum::<usize>()
    );
}
