use std::{
    cell::LazyCell,
    collections::HashMap,
    hash::{Hash, Hasher},
};

use aocd::prelude::*;

#[aocd(2025, 3)]
fn main() {
    let mut max_joltage_p1 = 0;
    let mut max_joltage_p2 = 0;
    for bank in input!().split_whitespace() {
        let batteries: Vec<u64> = bank
            .chars()
            .map(|c| u64::from(c.to_digit(10).unwrap()))
            .rev()
            .collect();
        max_joltage_p1 += max_joltage(&batteries, 2);
        max_joltage_p2 += max_joltage(&batteries, 12);
    }
    submit!(1, max_joltage_p1);
    submit!(2, max_joltage_p2);
}

static mut CACHE: LazyCell<HashMap<u64, u64>> = LazyCell::new(HashMap::new);

#[allow(static_mut_refs)]
fn max_joltage(batteries: &[u64], k: u32) -> u64 {
    let mut hasher = std::hash::DefaultHasher::new();
    batteries.hash(&mut hasher);
    k.hash(&mut hasher);
    let hash = hasher.finish();

    if let Some(cached) = unsafe { CACHE.get(&hash).copied() } {
        cached
    } else {
        let x = max_joltage_rec(batteries, k);
        unsafe { CACHE.insert(hash, x) };
        x
    }
}

fn max_joltage_rec(batteries: &[u64], k: u32) -> u64 {
    if k < 1 || batteries.is_empty() || batteries.len() < k as usize {
        return 0;
    }
    if k == 1 && batteries.len() == 1 {
        return batteries[0];
    }

    let dont_pick = max_joltage(&batteries[1..], k);
    let pick = max_joltage(&batteries[1..], k - 1) * 10 + batteries[0];

    dont_pick.max(pick)
}
