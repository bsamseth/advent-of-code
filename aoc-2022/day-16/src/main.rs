use aocd::*;
use itertools::Itertools;
use regex::Regex;
use std::collections::HashMap;

#[derive(Debug)]
struct Valve {
    connections: Vec<u8>,
    flow_rate: u16,
}

fn push_dedup(
    queue: &mut std::collections::BinaryHeap<State>,
    seen: &mut std::collections::HashSet<State>,
    state: State,
) {
    if seen.insert(state.clone()) {
        queue.push(state);
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum Choice {
    Open,
    Move(u8),
}

/*

R = Max flow rate: sum(valve.flow_rate for valve in valves)
F = Max flow: Max flow rate * max_time
T = Max time = 26


f(n) = g(n) + h(n),
g(n) = time * R
h(n) = (R - flow_rate) * time


Start: open = 0, loc = (0, 0), acc_flow = 0, time = 0   --- cost = max flow - max playout


Some step: open = 0b1011101, loc = (_, _), acc_flow = 55 time = 7


End: open = ~0, loc = (_, _), acc_flow = X, time = 26
*/

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct State {
    x: u8,
    y: u8,
    time: u8,
    acc_flow: u16,
    f: u16,
    open: u64,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other
            .f
            .cmp(&self.f)
            .then_with(|| other.time.cmp(&self.time))
            .then_with(|| self.acc_flow.cmp(&other.acc_flow))
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl State {
    fn new(
        x: u8,
        y: u8,
        time: u8,
        acc_flow: u16,
        open: u64,
        max_time: u8,
        max_flow_rate: u16,
        valves: &Vec<Valve>,
    ) -> Self {
        let mut s = Self {
            x: x.min(y),
            y: x.max(y),
            time,
            acc_flow,
            f: 0,
            open,
        };
        s.f = f(
            acc_flow,
            flow_rate(open, valves),
            time,
            max_time,
            max_flow_rate,
        );
        s
    }

    fn open(&self, v: u8) -> bool {
        self.open & (1u64 << v) != 0
    }
    fn all_open(&self, n_valves: u8) -> bool {
        self.open == (1 << n_valves) - 1
    }
}

fn f(acc_flow: u16, flow_rate: u16, time: u8, max_time: u8, max_flow_rate: u16) -> u16 {
    let g = max_flow_rate * (time as u16);
    let h = (max_flow_rate - flow_rate) * (time as u16);
    g + h
}

fn flow_rate(open: u64, valves: &Vec<Valve>) -> u16 {
    let mut flow_rate = 0;
    for (i, valve) in valves.iter().enumerate() {
        if open & (1u64 << i) != 0 {
            flow_rate += valve.flow_rate;
        }
    }
    flow_rate
}
fn maximal_playout(state: &State, max_flow_rate: u16, max_time: u8) -> u16 {
    state.acc_flow + max_flow_rate * ((max_time - state.time) as u16)
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
    let valve_index: HashMap<String, u8> = input
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

    let max_time = 26;
    let max_flow_rate = valves.iter().map(|v| v.flow_rate).sum();
    let all_open = (1u64 << valves.len()) - 1;
    let mut seen = std::collections::HashSet::new();
    let mut queue = std::collections::BinaryHeap::new();
    queue.push(State::new(
        valve_index["AA"],
        valve_index["AA"],
        0,
        0,
        valves.iter().enumerate().fold(0, |acc, (i, v)| {
            if v.flow_rate == 0 {
                acc | (1 << i)
            } else {
                acc
            }
        }),
        max_time,
        max_flow_rate,
        &valves,
    ));

    let mut t = 0;
    let mut max_found = 0;
    while let Some(state) = queue.pop() {
        if t < state.time {
            t = state.time;
            println!("time {:?}", t);
        }
        if state.open == all_open {
            println!("{}", state.acc_flow);
            break;
        }
        let fr = flow_rate(state.open, &valves);

        max_found = max_found.max(state.acc_flow);
        if maximal_playout(&state, max_flow_rate, max_time) < max_found {
            continue;
        }

        let x_choices = vec![Choice::Open]
            .into_iter()
            .chain(
                valves[state.x as usize]
                    .connections
                    .iter()
                    .map(|&i| Choice::Move(i)),
            )
            .collect::<Vec<_>>();
        let y_choices = vec![Choice::Open]
            .into_iter()
            .chain(
                valves[state.y as usize]
                    .connections
                    .iter()
                    .map(|&i| Choice::Move(i)),
            )
            .collect::<Vec<_>>();

        for (c1, c2) in x_choices.into_iter().cartesian_product(y_choices) {
            let mut new_open = state.open;
            let mut new_x = state.x;
            let mut new_y = state.y;
            match (c1, c2) {
                (Choice::Open, Choice::Open) if !state.open(state.x) && !state.open(state.y) => {
                    new_open |= (1 << state.x) | (1 << state.y);
                }
                (Choice::Open, Choice::Move(i)) if !state.open(state.x) => {
                    new_open |= 1 << state.x;
                    new_y = i;
                }
                (Choice::Move(i), Choice::Open) if !state.open(state.y) => {
                    new_open |= 1 << state.y;
                    new_x = i;
                }
                (Choice::Move(i), Choice::Move(j)) => {
                    new_x = i;
                    new_y = j;
                }
                _ => continue,
            }

            let new_state = if new_open == all_open {
                State::new(
                    0,
                    0,
                    state.time + 1,
                    state.acc_flow + fr + max_flow_rate * (max_time - state.time - 1) as u16,
                    all_open,
                    max_time,
                    max_flow_rate,
                    &valves,
                )
            } else {
                State::new(
                    new_x,
                    new_y,
                    state.time + 1,
                    state.acc_flow + fr,
                    new_open,
                    max_time,
                    max_flow_rate,
                    &valves,
                )
            };

            max_found = max_found.max(new_state.acc_flow);
            if maximal_playout(&new_state, max_flow_rate, max_time) >= max_found {
                push_dedup(&mut queue, &mut seen, new_state);
            }
        }
    }
}
