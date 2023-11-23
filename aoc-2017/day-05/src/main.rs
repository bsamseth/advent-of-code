use aocd::prelude::*;

enum Part {
    One,
    Two,
}

fn simulate(mut jumps: Vec<isize>, part: Part) -> usize {
    let mut total_jumps = 0;
    let mut pos: usize = 0;
    while pos < jumps.len() {
        let jump = jumps[pos];

        jumps[pos] += match part {
            Part::Two if jump >= 3 => -1,
            _ => 1,
        };
        pos = pos.wrapping_add_signed(jump);
        total_jumps += 1;
    }
    total_jumps
}

#[aocd(2017, 5)]
fn main() {
    let jumps = input!()
        .lines()
        .map(str::parse)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    submit!(1, simulate(jumps.clone(), Part::One));
    submit!(2, simulate(jumps.clone(), Part::Two));
}
