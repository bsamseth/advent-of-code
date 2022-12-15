use aocd::*;
use regex::Regex;
use std::collections::HashSet;

#[aocd(2022, 15)]
fn main() {
    let input = input!();
    let target_y = 2000000;
    // let target_y = 10;
    // let input = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
    // Sensor at x=9, y=16: closest beacon is at x=10, y=16
    // Sensor at x=13, y=2: closest beacon is at x=15, y=3
    // Sensor at x=12, y=14: closest beacon is at x=10, y=16
    // Sensor at x=10, y=20: closest beacon is at x=10, y=16
    // Sensor at x=14, y=17: closest beacon is at x=10, y=16
    // Sensor at x=8, y=7: closest beacon is at x=2, y=10
    // Sensor at x=2, y=0: closest beacon is at x=2, y=10
    // Sensor at x=0, y=11: closest beacon is at x=2, y=10
    // Sensor at x=20, y=14: closest beacon is at x=25, y=17
    // Sensor at x=17, y=20: closest beacon is at x=21, y=22
    // Sensor at x=16, y=7: closest beacon is at x=15, y=3
    // Sensor at x=14, y=3: closest beacon is at x=15, y=3
    // Sensor at x=20, y=1: closest beacon is at x=15, y=3";

    let number_regex = Regex::new(r"-?\d+").unwrap();
    let mut coords = number_regex
        .find_iter(&input)
        .map(|m| m.as_str().parse::<i32>().unwrap());
    let mut sensors = Vec::new();
    let mut beacons = Vec::new();
    while let Some(sensor_x) = coords.next() {
        let sensor_y = coords.next().unwrap();
        sensors.push((sensor_x, sensor_y));
        let beacon_x = coords.next().unwrap();
        let beacon_y = coords.next().unwrap();
        beacons.push((beacon_x, beacon_y));
    }

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
            if occupied.contains(&(x, target_y)) {
                continue;
            }
            empty_spaces_on_target_y.insert(x);
        }
    }
    // println!("{}", empty_spaces_on_target_y.len());
    submit!(1, empty_spaces_on_target_y.len());
    // submit!(
    //     1,
    //     empty_spaces.iter().filter(|(_, y)| *y == 2000000).count()
    // );
}
