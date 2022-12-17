use aocd::*;
use regex::Regex;
use std::collections::HashMap;

#[derive(Debug)]
struct Valve {
    connections: Vec<usize>,
    flow_rate: u32,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Node {
    at: usize,
    time: u16,
    open: u64,
    acc_flow: u32,
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other
            .time
            .cmp(&self.time)
            .then_with(|| self.acc_flow.cmp(&other.acc_flow))
            .then_with(|| self.open.cmp(&other.open))
            .then_with(|| self.at.cmp(&other.at))
    }
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Node {
    fn is_open(&self) -> bool {
        self.open & (1 << self.at) != 0
    }

    fn all_open(&self, valves: &Vec<Valve>) -> bool {
        self.open == (1 << valves.len()) - 1
    }

    fn maximal_playout(&self, valves: &Vec<Valve>, max_time: u16) -> u32 {
        let mut acc_flow = self.acc_flow;
        let mut flow_rate = self.flow_rate(valves);
        let mut time = self.time;
        let mut valves: Vec<_> = valves
            .iter()
            .enumerate()
            .filter(|(i, _)| self.open & (1 << i) == 0)
            .map(|(_, v)| v.flow_rate)
            .collect();
        valves.sort();
        for valve_flow_rate in valves.iter().rev() {
            if time < max_time {
                time += 1;
                acc_flow += flow_rate;
                flow_rate += valve_flow_rate;
            }
        }
        acc_flow += flow_rate * ((max_time - time) as u32);
        acc_flow
    }

    fn flow_rate(&self, valves: &Vec<Valve>) -> u32 {
        let mut flow_rate = 0;
        for (i, valve) in valves.iter().enumerate() {
            if self.open & (1 << i) != 0 {
                flow_rate += valve.flow_rate;
            }
        }
        flow_rate
    }
}

fn push_dedup(
    queue: &mut std::collections::BinaryHeap<Node>,
    seen: &mut std::collections::HashSet<Node>,
    node: Node,
) {
    if seen.insert(node.clone()) {
        queue.push(node);
    }
}

#[aocd(2022, 16)]
fn main() {
    let input = input!();
    // let input = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
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
    let valve_index: HashMap<String, usize> = input
        .lines()
        .map(|line| valve_regex.find(line).unwrap().as_str().to_string())
        .zip(0..)
        .collect();
    let valves: Vec<Valve> = input
        .lines()
        .map(|line| Valve {
            connections: valve_regex
                .find_iter(line)
                .skip(1)
                .map(|m| valve_index[m.as_str()])
                .collect(),
            flow_rate: number_regex.find(line).unwrap().as_str().parse().unwrap(),
        })
        .collect();

    let mut seen = std::collections::HashSet::new();
    let mut queue = std::collections::BinaryHeap::new();
    queue.push(Node {
        at: valve_index["AA"],
        // last: None,
        time: 0,
        open: valves.iter().enumerate().fold(0, |acc, (i, v)| {
            if v.flow_rate == 0 {
                acc | (1 << i)
            } else {
                acc
            }
        }),
        acc_flow: 0,
    });
    println!("number of valves {:?}", valves.len());
    let max_time = 30;
    let mut t = 0;
    let mut max_found = 0;
    while let Some(node) = queue.pop() {
        if t < node.time {
            t = node.time;
            println!("time {:?}", t);
        }
        if node.time == max_time {
            println!("{}", node.acc_flow);
            submit!(1, node.acc_flow);
            break;
        }

        let remaining_time = (max_time - node.time) as u32;

        max_found = max_found.max(node.acc_flow);
        if node.maximal_playout(&valves, max_time) < max_found {
            continue;
        }

        let flow_rate = node.flow_rate(&valves);

        // If all valves are open, we can't open any more.
        if node.all_open(&valves) {
            push_dedup(
                &mut queue,
                &mut seen,
                Node {
                    at: node.at,
                    time: max_time,
                    open: node.open,
                    acc_flow: node.acc_flow + flow_rate * remaining_time,
                },
            );
            max_found = max_found.max(node.acc_flow + flow_rate * remaining_time);
            continue;
        }

        // Option 1: Stay put, opening the valve if it's closed. This should not be searched,
        // as if we want to move later we should have done so here.
        let flow_rate_if_opened = flow_rate
            + if node.is_open() {
                0
            } else {
                valves[node.at].flow_rate
            };
        push_dedup(
            &mut queue,
            &mut seen,
            Node {
                at: node.at,
                time: max_time,
                open: node.open | (1 << node.at),
                acc_flow: node.acc_flow + flow_rate + (remaining_time - 1) * flow_rate_if_opened,
            },
        );
        max_found =
            max_found.max(node.acc_flow + flow_rate * (remaining_time - 1) + flow_rate_if_opened);

        // Option 2: Leave the valve as-is, and go to a connecting valve.
        for connection in &valves[node.at as usize].connections {
            push_dedup(
                &mut queue,
                &mut seen,
                Node {
                    at: *connection,
                    time: node.time + 1,
                    open: node.open,
                    acc_flow: node.acc_flow + flow_rate,
                },
            );
        }

        // Option 3: Open the valve, and go to a connecting valve.
        if !node.is_open() {
            for connection in &valves[node.at as usize].connections {
                push_dedup(
                    &mut queue,
                    &mut seen,
                    Node {
                        at: *connection,
                        time: node.time + 2,
                        open: node.open | (1 << node.at),
                        acc_flow: node.acc_flow + flow_rate + flow_rate_if_opened,
                    },
                );
            }
        }
    }
}
