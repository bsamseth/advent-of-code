use aocd::prelude::*;

#[aocd(2024, 2)]
fn main() {
    let mut p1 = 0;
    let mut p2 = 0;
    for line in input!().lines() {
        let levels = line
            .split_whitespace()
            .map(|x| x.parse::<i32>().unwrap())
            .collect::<Vec<_>>();
        if check_levels(&levels) {
            p1 += 1;
        }

        for skip in 0..levels.len() {
            let levels = levels
                .iter()
                .enumerate()
                .filter(|(i, _)| *i != skip)
                .map(|(_, x)| *x)
                .collect::<Vec<_>>();
            if check_levels(&levels) {
                p2 += 1;
                break;
            }
        }
    }
    submit!(1, p1);
    submit!(2, p2);
}

fn check_levels(levels: &[i32]) -> bool {
    let diffs = levels
        .iter()
        .zip(levels.iter().skip(1))
        .map(|(a, b)| b - a)
        .collect::<Vec<_>>();
    diffs
        .iter()
        .all(|x| (x.signum() == diffs[0].signum()) && (1..=3).contains(&x.abs()))
}
