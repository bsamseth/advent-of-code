use aocd::prelude::*;
use num::complex::Complex;

#[aocd(2023, 18)]
fn main() {
    let p1_instructions = input!()
        .lines()
        .map(|l| {
            let mut parts = l.split_whitespace();
            let dir = parts.next().unwrap();
            let count = parts.next().unwrap().parse::<i64>().unwrap();

            match dir {
                "U" => Complex::new(0, count),
                "D" => Complex::new(0, -count),
                "L" => Complex::new(-count, 0),
                "R" => Complex::new(count, 0),
                _ => panic!("Unknown direction in input"),
            }
        })
        .collect::<Vec<_>>();

    let p2_instructions = input!()
        .lines()
        .map(|l| {
            let hex = l.split_once('#').unwrap().1;

            let count = i64::from_str_radix(&hex[..hex.len() - 2], 16).unwrap();
            let dir = hex.as_bytes()[hex.len() - 2] as char;

            match dir {
                '0' => Complex::new(count, 0),
                '1' => Complex::new(0, -count),
                '2' => Complex::new(-count, 0),
                '3' => Complex::new(0, count),
                _ => panic!("Unknown direction in input"),
            }
        })
        .collect::<Vec<_>>();

    submit!(1, solve(&p1_instructions));
    submit!(2, solve(&p2_instructions));
}

fn solve(instructions: &[Complex<i64>]) -> u64 {
    let mut n_boundary_points = 0;
    let mut point = Complex::new(0, 0);
    let mut boundary_points = Vec::with_capacity(instructions.len());
    for &i in instructions {
        point += i;
        boundary_points.push(point);
        n_boundary_points += i.re.unsigned_abs() + i.im.unsigned_abs();
    }

    let interior_points = shoelace_area(&boundary_points) - n_boundary_points / 2 + 1;
    n_boundary_points + interior_points
}

fn shoelace_area(p: &[Complex<i64>]) -> u64 {
    let mut area = 0;
    for i in 0..p.len() {
        let j = (i + 1) % p.len();
        area += (p[i].im + p[j].im) * (p[i].re - p[j].re);
    }
    area.unsigned_abs() / 2
}
