use aocd::prelude::*;

struct Race {
    time: u64,
    record: u64,
}

impl Race {
    fn winning_bounds(&self) -> (u64, u64) {
        let t = self.time as f64;
        let s = self.record as f64;
        let z = (t * t - 4f64 * s).sqrt();
        let lower = ((t - z) / 2f64 + 1f64).floor() as u64;
        let upper = ((t + z) / 2f64 - 1f64).ceil() as u64;
        (lower, upper)
    }

    fn winning_ways(&self) -> u64 {
        let (lower, upper) = self.winning_bounds();
        upper - lower + 1
    }
}

#[aocd(2023, 6)]
fn main() {
    let input = input!()
        .lines()
        .map(|l| {
            l.split_whitespace()
                .filter_map(|s| s.parse::<u64>().ok())
                .collect::<Vec<u64>>()
        })
        .collect::<Vec<_>>();

    let races = input[0]
        .iter()
        .zip(input[1].iter())
        .map(|(t, s)| Race {
            time: *t,
            record: *s,
        })
        .collect::<Vec<_>>();

    let part1 = races
        .iter()
        .map(|r| r.winning_ways())
        .reduce(|a, b| a * b)
        .unwrap();

    submit!(1, part1);

    let part2_input = input!()
        .lines()
        .map(|l| {
            l.split_whitespace()
                .skip(1)
                .collect::<String>()
                .parse::<u64>()
                .unwrap()
        })
        .collect::<Vec<_>>();
    let p2_race = Race {
        time: part2_input[0],
        record: part2_input[1],
    };

    submit!(2, p2_race.winning_ways());
}
