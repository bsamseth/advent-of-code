use aocd::*;
use itertools::Itertools;
use regex::Regex;
use std::cell::RefCell;
use std::collections::HashMap;

fn parse_input(input: &str) -> (Vec<u8>, HashMap<u8, u16>, HashMap<(u8, u8), u16>) {
    let re = Regex::new(r"Valve (\w+) .*=(\d+);.* valves? (.*)").unwrap();

    let input = input.lines().map(|l| l.trim()).sorted().join("\n");

    let mut valve_names = HashMap::new();
    let mut flows = HashMap::new();
    let mut connections = HashMap::new();
    for (i, cap) in re.captures_iter(&input).enumerate() {
        let name = &cap[1];
        let flow = cap[2].parse::<u16>().unwrap();
        cap[3].split(", ").for_each(|s| {
            connections.insert((name.to_string(), s.to_string()), 1);
        });

        valve_names.insert(name.to_string(), i as u8);
        if flow > 0 {
            flows.insert(i as u8, flow);
        }
    }
    let valves = (0..(valve_names.len() as u8)).collect::<Vec<_>>();

    let mut distances: HashMap<(u8, u8), u16> = connections
        .iter()
        .map(|((k, v), _)| ((valve_names[k], valve_names[v]), 1))
        .collect();

    // Use Floyd-Warshall algorithm to calculate the distance between all pairs of nodes.
    for tripple in (0..3).map(|_| valves.iter()).multi_cartesian_product() {
        let (&k, &i, &j) = (tripple[0], tripple[1], tripple[2]);
        if k == i || k == j || i == j {
            continue;
        }
        let d_ij = *distances.get(&(i, j)).unwrap_or(&1000);
        let d_ik = *distances.get(&(i, k)).unwrap_or(&1000);
        let d_kj = *distances.get(&(k, j)).unwrap_or(&1000);

        distances.insert((i, j), d_ij.min(d_ik + d_kj));
    }

    (valves, flows, distances)
}

std::thread_local! {
    static CACHE: RefCell<HashMap<(u8, u8, u64, bool), u16>> = RefCell::new(HashMap::new());
}

fn search(
    time: u8,
    at: u8,
    open: u64,
    elephant: bool,
    valves: &Vec<u8>,
    flows: &HashMap<u8, u16>,
    distances: &HashMap<(u8, u8), u16>,
) -> u16 {
    let cached_result =
        CACHE.with(|cache| cache.borrow().get(&(time, at, open, elephant)).cloned());
    if let Some(result) = cached_result {
        return result;
    }

    let mut max = 0;
    for &v in valves.iter() {
        if open & (1u64 << v) != 0 {
            continue;
        }
        let flow_at_v = flows.get(&v).unwrap();
        let distance = *distances.get(&(at, v)).unwrap();

        if distance >= time as u16 {
            continue;
        }

        let remaining_time_if_moving_to_v = (time as u16) - distance - 1;

        let flow = flow_at_v * remaining_time_if_moving_to_v
            + search(
                time - (distance as u8) - 1,
                v,
                open | (1 << v),
                elephant,
                valves,
                flows,
                distances,
            );

        max = max.max(flow);
    }

    // If we have an elephant, check how much it would help given that we have opened what we have opened.
    // We search the elephant from its starting time, but with the same open valves as we have now.
    if elephant {
        max = max.max(search(26, 0, open, false, valves, flows, distances));
    }

    CACHE.with(|cache| cache.borrow_mut().insert((time, at, open, elephant), max));
    max
}

#[aocd(2022, 16)]
fn main() {
    let input = input!();
    let (valves, flows, distances) = parse_input(&input);

    // Let all broken valves be marked as already open.
    let open = valves.iter().fold(0u64, |acc, v| {
        if flows.get(v).is_none() {
            acc | (1 << v)
        } else {
            acc
        }
    });

    submit!(1, search(30, 0, open, false, &valves, &flows, &distances));
    submit!(2, search(26, 0, open, true, &valves, &flows, &distances));
}
