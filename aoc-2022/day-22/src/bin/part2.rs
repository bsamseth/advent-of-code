use std::collections::HashMap;

use aocd::*;
use num::complex::Complex;
use regex::Regex;

fn wrap(pos: Complex<i32>, dir: Complex<i32>) -> (Complex<i32>, Complex<i32>) {
    let (x, y) = (pos.re, pos.im);
    match (dir.re, dir.im, x / 50, y / 50) {
        (0, 1, 0, _) => (Complex::new(149 - x, 99), Complex::new(0, -1)),
        (0, 1, 1, _) => (Complex::new(49, x + 50), Complex::new(-1, 0)),
        (0, 1, 2, _) => (Complex::new(149 - x, 149), Complex::new(0, -1)),
        (0, 1, 3, _) => (Complex::new(149, x - 100), Complex::new(-1, 0)),
        (0, -1, 0, _) => (Complex::new(149 - x, 0), Complex::new(0, 1)),
        (0, -1, 1, _) => (Complex::new(100, x - 50), Complex::new(1, 0)),
        (0, -1, 2, _) => (Complex::new(149 - x, 50), Complex::new(0, 1)),
        (0, -1, 3, _) => (Complex::new(0, x - 100), Complex::new(1, 0)),
        (1, 0, _, 0) => (Complex::new(0, y + 100), Complex::new(1, 0)),
        (1, 0, _, 1) => (Complex::new(100 + y, 49), Complex::new(0, -1)),
        (1, 0, _, 2) => (Complex::new(-50 + y, 99), Complex::new(0, -1)),
        (-1, 0, _, 0) => (Complex::new(50 + y, 50), Complex::new(0, 1)),
        (-1, 0, _, 1) => (Complex::new(100 + y, 0), Complex::new(0, 1)),
        (-1, 0, _, 2) => (Complex::new(199, y - 100), Complex::new(-1, 0)),
        _ => unreachable!(),
    }
}

fn password(pos: Complex<i32>, dir: Complex<i32>) -> usize {
    (pos.re as usize + 1) * 1000
        + 4 * (pos.im as usize + 1)
        + ([
            Complex::new(0, 1),
            Complex::new(1, 0),
            Complex::new(0, -1),
            Complex::new(-1, 0),
        ]
        .iter()
        .position(|&x| x == dir)
        .unwrap())
}

#[aocd(2022, 22)]
fn main() {
    let input = input!();
    let (map_string, path) = match input.split("\n\n").collect::<Vec<_>>()[..] {
        [map, path] => (map, path),
        _ => panic!("Invalid input"),
    };

    let mut pos = Complex::new(0, map_string.find('.').unwrap() as i32);
    let mut dir = Complex::new(0, 1i32);

    let mut map = HashMap::new();
    for (i, line) in map_string.lines().enumerate() {
        for (j, char) in line.chars().enumerate() {
            if char == '#' || char == '.' {
                map.insert(Complex::new(i as i32, j as i32), char);
            }
        }
    }

    let re = Regex::new(r"([LR]|\d+)").unwrap();
    for inst in re.captures_iter(path) {
        match &inst[1] {
            "L" => dir *= Complex::new(0, 1),
            "R" => dir *= Complex::new(0, -1),
            n => {
                let n = n.parse::<i32>().unwrap();
                for _ in 0..n {
                    let (mut p, mut d) = (pos + dir, dir);
                    if map.get(&p).is_none() {
                        (p, d) = wrap(p, d)
                    }
                    if let Some('.') = map.get(&p) {
                        (pos, dir) = (p, d)
                    }
                }
            }
        }
    }

    submit!(2, password(pos, dir));
}
