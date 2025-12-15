use aocd::prelude::*;

#[aocd(2025, 2)]
fn main() {
    let ranges = input!()
        .split(',')
        .map(|range| {
            let (start, end) = range.split_once('-').unwrap();
            start.parse::<u64>().unwrap()..=end.parse().unwrap()
        })
        .collect::<Vec<_>>();

    let invalid_sum_1: u64 = ranges
        .clone()
        .into_iter()
        .map(|range| {
            range
                .filter(|n| is_invalid_p1(*n))
                .inspect(|n| println!("{n}"))
                .sum::<u64>()
        })
        .sum();
    let invalid_sum_2: u64 = ranges
        .into_iter()
        .map(|range| {
            range
                .filter(|n| is_invalid_p2(*n))
                .inspect(|n| println!("{n}"))
                .sum::<u64>()
        })
        .sum();
    submit!(1, invalid_sum_1);
    submit!(2, invalid_sum_2);
}

fn is_invalid_p1(n: u64) -> bool {
    let s = format!("{n}");
    let s = s.as_bytes();
    if s.len() % 2 != 0 {
        return false;
    }
    let (a, b) = s.split_at(s.len() / 2);
    a == b
}
fn is_invalid_p2(n: u64) -> bool {
    let s = format!("{n}");
    let s = s.as_bytes();
    'outer: for chunk_size in 1..=(s.len() / 2) {
        let mut chunks = s.chunks_exact(chunk_size);
        if !chunks.remainder().is_empty() {
            continue;
        }

        let first = chunks.next().unwrap();
        for chunk in chunks {
            if first != chunk {
                continue 'outer;
            }
        }
        return true;
    }

    false
}
