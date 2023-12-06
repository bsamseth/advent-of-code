use aocd::prelude::*;
use std::str::FromStr;

#[aocd(2023, 5)]
fn main() {
    let input = input!();
    let mut input = input.split("\n\n");
    let seed_numbers = input
        .next()
        .unwrap()
        .split_whitespace()
        .filter_map(|n| n.parse::<usize>().ok())
        .collect::<Vec<_>>();
    let mappings: Vec<Mapping> = input.map(|s| s.parse()).collect::<Result<_, _>>().unwrap();

    let mut p1_seeds = seed_numbers
        .iter()
        .map(|&n| Range { from: n, to: n + 1 })
        .collect::<Vec<_>>();
    let mut p2_seeds = seed_numbers
        .iter()
        .step_by(2)
        .zip(seed_numbers.iter().skip(1).step_by(2))
        .map(|(&from, &size)| Range {
            from,
            to: from + size,
        })
        .collect::<Vec<_>>();

    for mapping in &mappings {
        for seeds in [&mut p1_seeds, &mut p2_seeds] {
            *seeds = seeds
                .iter()
                .flat_map(|range| mapping.map(range))
                .collect::<Vec<_>>();
        }
    }

    submit!(1, p1_seeds.iter().map(|r| r.from).min().unwrap());
    submit!(2, p2_seeds.iter().map(|r| r.from).min().unwrap());
}

#[derive(Debug, Clone)]
struct Range {
    from: usize,
    to: usize,
}

#[derive(Debug)]
struct MapRange {
    dst_min: usize,
    src_min: usize,
    size: usize,
}

#[derive(Debug)]
struct Mapping {
    ranges: Vec<MapRange>,
}

impl MapRange {
    fn map(&self, src: &Range) -> [Option<Range>; 3] {
        let mut result = [None, None, None];

        // Check for any parts of the range that are below the mapping region:
        if src.from < self.src_min {
            result[0] = Some(Range {
                from: src.from,
                to: std::cmp::min(src.to, self.src_min),
            });
        }
        // Check for an overlap between the regions [src.from, src.to) and [self.src_min, self.src_min+self.size):
        if src.from < self.src_min + self.size && src.to > self.src_min {
            let f = std::cmp::max(src.from, self.src_min);
            let t = std::cmp::min(src.to, self.src_min + self.size);
            result[1] = Some(Range {
                from: self.dst_min + (f - self.src_min),
                to: self.dst_min + (t - self.src_min),
            });
        }

        // Check for any parts of the range that are above the mapping region:
        if src.to > self.src_min + self.size {
            result[2] = Some(Range {
                from: std::cmp::max(src.from, self.src_min + self.size),
                to: src.to,
            });
        }

        result
    }
}

impl<'a> Mapping {
    fn map(&'a self, src: &'a Range) -> impl Iterator<Item = Range> + 'a {
        let mut left = vec![src.clone()];
        let mut done = vec![];
        for map_range in &self.ranges {
            for _ in 0..left.len() {
                let range = left.remove(0);
                let [under, in_, over] = map_range.map(&range);
                if let Some(in_) = in_ {
                    done.push(in_);
                }
                if let Some(under) = under {
                    left.push(under);
                }
                if let Some(over) = over {
                    left.push(over);
                }
            }
        }

        done.extend(left);
        done.into_iter()
    }
}

impl FromStr for MapRange {
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
