use aocd::*;
use regex::Regex;
use std::collections::HashMap;

#[derive(Debug)]
struct Valve {
    name: String,
    connections: Vec<String>,
    flow_rate: u32,
    open: std::cell::Cell<bool>,
}

fn max_flow(
    valves: &HashMap<String, Valve>,
    valve: &Valve,
    last_valve: Option<&Valve>,
    flow_rate: u32,
    remaining_minutes: u32,
) -> u32 {
    if remaining_minutes == 0 || valves.values().all(|v| v.open.get()) {
        return flow_rate * remaining_minutes;
    }

    // Option 1: Just stay put, opening the valve if it's closed.
    let max_open_and_stay = if valve.open.get() {
        flow_rate * remaining_minutes
    } else {
        flow_rate + (flow_rate + valve.flow_rate) * (remaining_minutes - 1)
    };

    // Option 2: Leave the valve as-is, and go to a connecting valve.
    let mut max_just_walk = 0;
    for connection in &valve.connections {
        if connection == last_valve.map(|v| &v.name).unwrap_or(&String::new()) {
            continue;
        }
        max_just_walk = max_just_walk.max(
            flow_rate
                + max_flow(
                    valves,
                    valves.get(connection).unwrap(),
                    Some(valve),
                    flow_rate,
                    remaining_minutes - 1,
                ),
        );
    }

    // Option 3: Open the valve, and go to a connecting valve. Only an option if remaining minutes > 1.
    let mut max_open_and_walk = 0;
    if remaining_minutes > 1 && !valve.open.get() {
        valve.open.set(true);
        for connection in &valve.connections {
            max_open_and_walk = max_open_and_walk.max(
                flow_rate
                    + (flow_rate + valve.flow_rate)
                    + max_flow(
                        valves,
                        valves.get(connection).unwrap(),
                        Some(valve),
                        flow_rate + valve.flow_rate,
                        remaining_minutes - 2,
                    ),
            );
        }
        valve.open.set(false);
    }
    max_open_and_stay.max(max_just_walk).max(max_open_and_walk)
}

#[aocd(2022, 16)]
fn main() {
    let input = input!();
    //     let input = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
    // Valve BB has flow rate=13; tunnels lead to valves CC, AA
    // Valve CC has flow rate=2; tunnels lead to valves DD, BB
    // Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
    // Valve EE has flow rate=3; tunnels lead to valves FF, DD
    // Valve FF has flow rate=0; tunnels lead to valves EE, GG
    // Valve GG has flow rate=0; tunnels lead to valves FF, HH
    // Valve HH has flow rate=22; tunnel leads to valve GG
    // Valve II has flow rate=0; tunnels lead to valves AA, JJ
    // Valve JJ has flow rate=21; tunnel leads to valve II";

    let valve_regex = Regex::new(r"[A-Z]{2}").unwrap();
    let number_regex = Regex::new(r"\d+").unwrap();
    let valves: HashMap<String, Valve> = HashMap::from_iter(input.lines().map(|line| {
        let mut valves = valve_regex.find_iter(line).map(|m| m.as_str().to_string());
        let valve = Valve {
            name: valves.next().unwrap(),
            connections: valves.collect(),
            flow_rate: number_regex.find(line).unwrap().as_str().parse().unwrap(),
            open: std::cell::Cell::new(false),
        };
        (valve.name.clone(), valve)
    }));

    let part1 = max_flow(&valves, &valves["AA"], None, 0, 30);
    submit!(1, part1);
}
