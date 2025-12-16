use std::{collections::VecDeque, ops::RangeInclusive};

use aocd::prelude::*;

#[aocd(2025, 5)]
fn main() {
    let input = input!();
    let (ranges, ids) = input.split_once("\n\n").unwrap();
    let ranges = ranges
        .split_whitespace()
        .map(|line| {
            line.split_once('-')
                .map(|(a, b)| a.parse::<u64>().unwrap()..=b.parse().unwrap())
                .unwrap()
        })
        .collect::<VecDeque<_>>();
    let ids: Vec<u64> = ids.split_whitespace().map(|s| s.parse().unwrap()).collect();

    let fresh_count = ids
        .iter()
        .filter(|id| ranges.iter().any(|range| range.contains(*id)))
        .count();

    let ranges = merge_ranges(ranges);
    let total: usize = ranges.into_iter().map(std::iter::Iterator::count).sum();

    submit!(1, fresh_count);
    submit!(2, total);
}

fn merge_ranges(mut ranges: VecDeque<RangeInclusive<u64>>) -> VecDeque<RangeInclusive<u64>> {
    'outer: loop {
        for _ in 0..ranges.len() {
            let range = ranges.pop_back().unwrap();
            for other in &mut ranges {
                if range.contains(other.start())
                    || range.contains(other.end())
                    || other.contains(range.start())
                    || other.contains(range.end())
                {
                    let _old = std::mem::replace(
                        other,
                        (*range.start()).min(*other.start())..=(*range.end()).max(*other.end()),
                    );
                    continue 'outer;
                }
            }

            ranges.push_front(range);
        }
        break;
    }

    ranges
}
