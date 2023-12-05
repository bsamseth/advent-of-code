use std::str::FromStr;

use aocd::prelude::*;

struct Range {
    dst_min: usize,
    src_min: usize,
    size: usize,
}

impl Range {
    fn map(&self, src: usize) -> Option<usize> {
        if src < self.src_min || src >= self.src_min + self.size {
            return None;
        }
        Some(self.dst_min + src - self.src_min)
    }
}

impl FromStr for Range {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts = s
            .split_whitespace()
            .map(|n| n.parse::<usize>())
            .collect::<Result<Vec<_>, _>>()
            .map_err(|_| ())?;

        assert_eq!(parts.len(), 3);

        Ok(Self {
            dst_min: parts[0],
            src_min: parts[1],
            size: parts[2],
        })
    }
}

struct Mapping {
    ranges: Vec<Range>,
}

impl Mapping {
    fn map(&self, src: usize) -> usize {
        self.ranges
            .iter()
            .find_map(|range| range.map(src))
            .unwrap_or(src)
    }
}

impl FromStr for Mapping {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let ranges = s
            .lines()
            .skip(1)
            .map(str::parse)
            .collect::<Result<Vec<_>, _>>()
            .map_err(|_| ())?;

        Ok(Self { ranges })
    }
}

#[aocd(2023, 5)]
fn main() {
    let input = input!();
    let mut input = input.split("\n\n");
    let mut seeds: Vec<usize> = input
        .next()
        .unwrap()
        .split_whitespace()
        .filter_map(|n| n.parse::<usize>().ok())
        .collect();

    let seed_ranges: Vec<(usize, usize)> = seeds
        .clone()
        .iter()
        .step_by(2)
        .zip(seeds.iter().skip(1).step_by(2))
        .map(|(&from, &size)| (from, size))
        .collect();

    let input: Vec<Mapping> = input.map(|s| s.parse()).collect::<Result<_, _>>().unwrap();

    for mapping in &input {
        seeds = seeds.iter().map(|&n| mapping.map(n)).collect();
    }

    submit!(1, seeds.iter().min().unwrap());

    let mut min_loc = usize::MAX;
    for (from, size) in seed_ranges {
        println!("doing seed range {} {}", from, size);
        for mut seed in from..from + size {
            for mapping in &input {
                seed = mapping.map(seed);
            }
            if seed < min_loc {
                min_loc = seed;
            }
        }
    }

    submit!(2, min_loc);
}
