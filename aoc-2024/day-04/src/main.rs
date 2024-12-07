#![allow(clippy::cast_sign_loss, clippy::cast_possible_wrap)]
use aocd::prelude::*;

#[aocd(2024, 4)]
fn main() {
    let input = input!();
    let width = input.lines().next().unwrap().len() as isize + 1;
    let c = |(i, j)| input.chars().nth((i * width + j) as usize);

    let mut p1 = 0;
    let mut p2 = 0;
    for i in 0..(input.lines().count() as isize) {
        for j in 0..width {
            for di in -1..=1 {
                for dj in -1..=1 {
                    if (0isize..4)
                        .map(|k| c((i + di * k, j + dj * k)))
                        .collect::<Option<String>>()
                        .is_some_and(|s| s == "XMAS")
                    {
                        p1 += 1;
                    }
                }
            }

            let a = (-1..=1)
                .map(|d| c((i + d, j + d)))
                .collect::<Option<String>>();
            let b = (-1..=1)
                .map(|d| c((i + d, j - d)))
                .collect::<Option<String>>();

            if a.is_some_and(|s| s == "MAS" || s == "SAM")
                && b.is_some_and(|s| s == "MAS" || s == "SAM")
            {
                p2 += 1;
            }
        }
    }

    submit!(1, p1);
    submit!(2, p2);
}
