use aocd::prelude::*;
use num::complex::Complex;
use std::collections::HashMap;

const NORTH: Complex<isize> = Complex::new(0, -1);
const EAST: Complex<isize> = Complex::new(1, 0);
const SOUTH: Complex<isize> = Complex::new(0, 1);
const WEST: Complex<isize> = Complex::new(-1, 0);

#[aocd(2023, 10)]
fn main() {
    let map = input!()
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars().enumerate().filter_map(move |(x, c)| match c {
                '.' => None,
                c => Some((Complex::new(x as isize, y as isize), c)),
            })
        })
        .collect::<HashMap<_, _>>();

    let start = *map.iter().find(|(_, &c)| c == 'S').unwrap().0;
    let mut boundary = vec![start];
    let mut prev = start;
    let mut curr = start_connection(&map, start);

    while curr != start {
        boundary.push(curr);

        let current_char = map.get(&curr).unwrap();
        let next = match *current_char {
            '|' => vec![NORTH, SOUTH],
            '-' => vec![WEST, EAST],
            '7' => vec![SOUTH, WEST],
            'F' => vec![SOUTH, EAST],
            'J' => vec![NORTH, WEST],
            'L' => vec![NORTH, EAST],
            _ => unreachable!(),
        }
        .iter()
        .map(|&dir| curr + dir)
        .find(|&c| c != prev)
        .unwrap();

        prev = curr;
        curr = next;
    }

    boundary.push(start);

    submit!(1, boundary.len() / 2);
    submit!(2, picks(&boundary));
}

fn picks(p: &[Complex<isize>]) -> isize {
    let area = shoelace(p).abs() / 2;
    let boundary = p.len() as isize;
    area - boundary / 2 + 1
}

fn shoelace(p: &[Complex<isize>]) -> isize {
    return p
        .iter()
        .zip(p.iter().skip(1))
        .fold(0, |a, (z1, z2)| a + (z1.im + z2.im) * (z1.re - z2.re));
}

fn start_connection(map: &HashMap<Complex<isize>, char>, start: Complex<isize>) -> Complex<isize> {
    for &dir in &[NORTH, EAST, SOUTH, WEST] {
        if let Some(&c) = map.get(&(start + dir)) {
            let con = start + dir;
            match dir {
                NORTH if c == '|' || c == '7' || c == 'F' => return con,
                SOUTH if c == '|' || c == 'J' || c == 'L' => return con,
                WEST if c == '-' || c == 'F' || c == 'L' => return con,
                EAST if c == '-' || c == '7' || c == 'J' => return con,
                _ => {}
            };
        }
    }
    unreachable!()
}
