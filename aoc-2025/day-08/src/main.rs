use std::collections::HashMap;

use aocd::prelude::*;

#[aocd(2025, 8)]
fn main() {
    let input = input!();

    let junction_boxes = input
        .split('\n')
        .map(JunctionBox::from_str)
        .collect::<Vec<_>>();
    let mut circuits: HashMap<CircuitId, CircuitSize> = junction_boxes
        .iter()
        .enumerate()
        .map(|(id, _)| (id as CircuitId, 1 as CircuitSize))
        .collect();
    let mut junction_to_circuit: HashMap<JunctionBox, CircuitId> = junction_boxes
        .iter()
        .enumerate()
        .map(|(id, j)| (*j, id as CircuitId))
        .collect();

    let mut distances = Vec::new();
    for (i, j1) in junction_boxes
        .iter()
        .enumerate()
        .take_while(|(i, _)| *i < junction_boxes.len() - 1)
    {
        for j2 in &junction_boxes[i + 1..] {
            distances.push((squared_dist(j1, j2), *j1, *j2));
        }
    }
    distances.sort_unstable();

    for (_, j1, j2) in &distances[..1000] {
        let c1 = *junction_to_circuit.get(j1).unwrap();
        let c2 = *junction_to_circuit.get(j2).unwrap();
        if c1 == c2 {
            continue;
        }

        for c in junction_to_circuit.values_mut().filter(|c| **c == c2) {
            *c = c1;
        }

        let c2_size = circuits.remove(&c2).unwrap();
        *circuits.get_mut(&c1).unwrap() += c2_size;
    }

    {
        let mut circuits: Vec<_> = circuits.iter().collect();
        circuits.sort_unstable_by_key(|(_id, count)| **count);
        let prod: u64 = circuits[circuits.len() - 3..]
            .iter()
            .map(|(_id, size)| *size)
            .product();

        submit!(1, prod);
    }

    for (_, j1, j2) in &distances[1000..] {
        let c1 = *junction_to_circuit.get(j1).unwrap();
        let c2 = *junction_to_circuit.get(j2).unwrap();
        if c1 == c2 {
            continue;
        }

        for c in junction_to_circuit.values_mut().filter(|c| **c == c2) {
            *c = c1;
        }

        let c2_size = circuits.remove(&c2).unwrap();
        *circuits.get_mut(&c1).unwrap() += c2_size;

        if circuits.len() == 1 {
            submit!(2, j1.x * j2.x);
            break;
        }
    }
}

type CircuitId = u64;
type CircuitSize = u64;

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
struct JunctionBox {
    x: i64,
    y: i64,
    z: i64,
}

impl JunctionBox {
    fn from_str(s: &str) -> Self {
        let numbers = s
            .splitn(3, ',')
            .map(|n| n.parse::<i64>().expect("parsing number"))
            .collect::<Vec<i64>>();
        JunctionBox {
            x: numbers[0],
            y: numbers[1],
            z: numbers[2],
        }
    }
}

fn squared_dist(a: &JunctionBox, b: &JunctionBox) -> i64 {
    let dx = b.x - a.x;
    let dy = b.y - a.y;
    let dz = b.z - a.z;
    dx * dx + dy * dy + dz * dz
}
