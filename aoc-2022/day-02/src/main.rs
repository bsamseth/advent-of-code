use aocd::*;

#[aocd(2022, 2)]
fn main() {
    let mut part1 = 0;
    let mut part2 = 0;
    for line in input!().lines() {
        let mut parts = line.split(" ").map(|s| match s {
            "A" | "X" => 0,
            "B" | "Y" => 1,
            "C" | "Z" => 2,
            _ => panic!("invalid input"),
        });
        let a: i32 = parts.next().unwrap();
        let b: i32 = parts.next().unwrap();

        // After trying for a bit to find a clean direct formula, this one was cleaner still.
        // Credit: https://github.com/udoprog/aoc2022/blob/main/years/2022/src/bin/d02.rs
        part1 += (2 - (a - b + 1).rem_euclid(3)) * 3 + b + 1;
        part2 += b * 3 + (a + b - 1).rem_euclid(3) + 1;
    }

    submit!(1, part1);
    submit!(2, part2);
}
