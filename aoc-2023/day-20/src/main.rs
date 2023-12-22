use std::collections::{HashMap, VecDeque};

use aocd::prelude::*;

#[aocd(2023, 20)]
fn main() {
    let input = input!().leak().trim();

    submit!(1, part1(input));
    submit!(2, part2(input));
}

fn part1(input: &'static str) -> usize {
    let mut circuit: Circuit = input.into();
    let (mut low, mut high) = (0, 0);
    for _ in 0..1000 {
        let (l, h, _) = circuit.press_button(None);
        low += l;
        high += h;
    }
    low * high
}

fn part2(input: &'static str) -> usize {
    let rx_input = input
        .lines()
        .find(|line| line.trim_end().ends_with("-> rx"))
        .unwrap()
        .split_once(" -> ")
        .unwrap()
        .0;
    assert!(rx_input.starts_with('&'));
    let rx_input = &rx_input[1..];
    let circuit: Circuit = input.into();

    circuit
        .predecessors(rx_input)
        .iter()
        .map(|&predecessor| run_until(circuit.clone(), predecessor, Pulse::High))
        .reduce(num::integer::lcm)
        .unwrap()
}

/// Press the button until the given module outputs the desired pulse.
fn run_until(mut circuit: Circuit, module: Label, desired: Pulse) -> usize {
    for presses in 1.. {
        let (_, _, pulse) = circuit.press_button(Some(module));
        if let Some(pulse) = pulse {
            if pulse == desired {
                return presses;
            }
        }
    }
    unreachable!()
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum Pulse {
    High,
    Low,
}

#[derive(Debug, Clone)]
struct Circuit(HashMap<Label, Module>);

#[derive(Debug, Clone)]
enum Module {
    Broadcaster(Outputs),
    FlipFlop(Outputs, FlipFlopState),
    Conjunction(Outputs, ConjunctionMemory),
}

#[derive(Debug, Clone, Copy)]
enum FlipFlopState {
    Off,
    On,
}

#[derive(Debug, Clone)]
struct ConjunctionMemory(HashMap<Label, Pulse>);

type Label = &'static str;
type Outputs = Vec<Label>;

impl Circuit {
    fn press_button(&mut self, watch: Option<Label>) -> (usize, usize, Option<Pulse>) {
        let mut queue = VecDeque::new();
        queue.push_back(("button", "broadcaster", Pulse::Low));

        let mut low = 0;
        let mut high = 0;
        let mut watch_result = None;

        while let Some((from, to, pulse)) = queue.pop_front() {
            if let Some(watch) = watch {
                if watch_result.is_none() && from == watch {
                    watch_result = Some(pulse);
                }
            }
            if to.is_empty() {
                continue;
            }

            if let Pulse::High = pulse {
                high += 1;
            } else {
                low += 1;
            }

            if let Some(module) = self.0.get_mut(to) {
                module.propagate(pulse, from, to, Enqueueable(&mut queue));
            }
        }

        (low, high, watch_result)
    }

    fn predecessors(&self, label: Label) -> Vec<Label> {
        self.0
            .iter()
            .filter(|(_, module)| {
                let outputs = match module {
                    Module::Broadcaster(outputs) => outputs,
                    Module::FlipFlop(outputs, _) => outputs,
                    Module::Conjunction(outputs, _) => outputs,
                };

                outputs.contains(&label)
            })
            .map(|(label, _)| *label)
            .collect()
    }
}

impl Module {
    fn propagate(
        &mut self,
        mut pulse: Pulse,
        from: Label,
        us: Label,
        mut queue: Enqueueable<(Label, Label, Pulse)>,
    ) {
        let outputs = match self {
            Self::Broadcaster(outputs) => outputs,
            Self::FlipFlop(outputs, state) => {
                if matches!(pulse, Pulse::High) {
                    return;
                }

                (*state, pulse) = match state {
                    FlipFlopState::Off => (FlipFlopState::On, Pulse::High),
                    FlipFlopState::On => (FlipFlopState::Off, Pulse::Low),
                };

                outputs
            }
            Self::Conjunction(outputs, memory) => {
                memory.0.insert(from, pulse);

                pulse = if memory.0.values().all(|&pulse| matches!(pulse, Pulse::High)) {
                    Pulse::Low
                } else {
                    Pulse::High
                };

                outputs
            }
        };

        for output in outputs {
            queue.enqueue((us, output, pulse));
        }
    }
}

struct Enqueueable<'a, T>(&'a mut VecDeque<T>);
impl<'a, T> Enqueueable<'a, T> {
    fn enqueue(&mut self, thing: T) {
        self.0.push_back(thing);
    }
}

impl From<&'static str> for Circuit {
    fn from(input: &'static str) -> Self {
        let mut modules = input
            .lines()
            .map(|line| {
                let (descriptor, outputs) = line.split_once(" -> ").unwrap();
                let outputs = outputs.split(", ").collect::<Vec<_>>();
                match descriptor.chars().next().unwrap() {
                    'b' => (descriptor, Module::Broadcaster(outputs)),
                    '%' => (
                        &descriptor[1..],
                        Module::FlipFlop(outputs, FlipFlopState::Off),
                    ),
                    '&' => (
                        &descriptor[1..],
                        Module::Conjunction(outputs, ConjunctionMemory(HashMap::new())),
                    ),
                    _ => unreachable!(),
                }
            })
            .collect::<HashMap<_, _>>();

        let conjunction_pairs = modules
            .iter()
            .flat_map(|(&label, module)| {
                let outputs = match module {
                    Module::Broadcaster(outputs) => outputs,
                    Module::FlipFlop(outputs, _) => outputs,
                    Module::Conjunction(outputs, _) => outputs,
                };

                outputs
                    .iter()
                    .filter_map(|&output| {
                        if let Some(Module::Conjunction(_, _)) = &modules.get(output) {
                            Some(output)
                        } else {
                            None
                        }
                    })
                    .map(move |output| (label, output))
            })
            .collect::<Vec<(Label, Label)>>();

        for (from, to) in conjunction_pairs {
            if let Some(Module::Conjunction(_, memory)) = modules.get_mut(to) {
                memory.0.insert(from, Pulse::Low);
            }
        }

        Self(modules)
    }
}
