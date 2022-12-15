use aocd::*;
use regex::Regex;
use std::collections::HashSet;

/// Keep track of a subset of the interval [0, max], possibly with any number of holes.
#[derive(Debug)]
struct Ranges {
    max: u64,
    ranges: Vec<(u64, u64)>,
}

impl Ranges {
    fn new(max: u64) -> Self {
        Self {
            ranges: vec![(0, max)],
            max,
        }
    }

    /// Remove the interval [from, to] from the ranges, inclusive of both ends.
    fn skip(&mut self, from: i32, to: i32) {
        let from = from.try_into().unwrap_or(0);
        let to = to.try_into().unwrap_or(0).min(self.max);
        let mut new_ranges = Vec::with_capacity(self.ranges.len());
        for &(a, b) in self.ranges.iter() {
            if from <= a && b <= to {
                // Range contained within new skip range, so we just delete this range.
                // Continue as we may have more ranges to delete.
                continue;
            } else if from <= b && a <= to {
                // Skip overlaps with range, so we shrink the range from the side that overlaps the skip.
                if a < from {
                    new_ranges.push((a, from - 1));
                }
                if to < b {
                    new_ranges.push((to + 1, b));
                }

                // If the skip is entirely within the range, no other ranges can overlap with it and we can break.
                if a <= from && to <= b {
                    break;
                }
            } else {
                // No overlap, so the range is unaffected.
                new_ranges.push((a, b));
            }
        }
        self.ranges = new_ranges;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Coord(i32, i32);

fn parse_sensors_and_beacons(input: &str) -> (Vec<Coord>, Vec<Coord>) {
    let number_regex = Regex::new(r"-?\d+").unwrap();
    let mut coords = number_regex
        .find_iter(input)
        .map(|m| m.as_str().parse::<i32>().unwrap());
    let mut sensors = Vec::new();
    let mut beacons = Vec::new();
    while let Some(sensor_x) = coords.next() {
        let sensor_y = coords.next().unwrap();
        sensors.push(Coord(sensor_x, sensor_y));
        let beacon_x = coords.next().unwrap();
        let beacon_y = coords.next().unwrap();
        beacons.push(Coord(beacon_x, beacon_y));
    }
    (sensors, beacons)
}

#[aocd(2022, 15)]
fn main() {
    let input = input!();
    let target_y = 2000000;
    let max_size = 4000000;

    let (sensors, beacons) = parse_sensors_and_beacons(&input);

    let occupied = sensors.iter().chain(beacons.iter()).collect::<HashSet<_>>();
    let mut empty_spaces_on_target_y = HashSet::new();
    for (sensor, beacon) in sensors.iter().zip(beacons.iter()) {
        let dist = (sensor.0 - beacon.0).abs() + (sensor.1 - beacon.1).abs();
        let dist_from_target_y = (sensor.1 - target_y).abs();
        let max_dx = dist - dist_from_target_y;
        if max_dx <= 0 {
            continue;
        }
        for x in sensor.0 - max_dx..=sensor.0 + max_dx {
            if !occupied.contains(&Coord(x, target_y)) {
                empty_spaces_on_target_y.insert(x);
            }
        }
    }
    submit!(1, empty_spaces_on_target_y.len());

    for y in 0..=max_size {
        let mut x_ranges = Ranges::new(max_size.try_into().unwrap());
        for (sensor, beacon) in sensors.iter().zip(beacons.iter()) {
            let dist = (sensor.0 - beacon.0).abs() + (sensor.1 - beacon.1).abs();
            let remaining_dist = dist - (sensor.1 - y).abs();
            if remaining_dist <= 0 {
                continue;
            }
            x_ranges.skip(sensor.0 - remaining_dist, sensor.0 + remaining_dist);
        }
        if let Some((a, b)) = x_ranges.ranges.first() {
            assert!(a == b);
            submit!(2, a * 4_000_000 + (y as u64));
            return;
        }
    }
}
