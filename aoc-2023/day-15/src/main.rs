use aocd::prelude::*;
use itertools::Itertools;

#[aocd(2023, 15)]
fn main() {
    let input = input!().leak();
    let steps = input.trim_end().split_terminator(',').collect_vec();
    let verification_number = steps.iter().map(|step| hash(step) as u64).sum::<u64>();
    submit!(1, verification_number);

    let mut boxes = vec![vec![]; 256];
    for step in steps {
        let step = Step::from(step);
        step.apply(&mut boxes[step.hash() as usize]);
    }
    let focus_power = boxes
        .iter()
        .enumerate()
        .flat_map(|(box_nr, box_)| {
            box_.iter().enumerate().map(move |(lens_nr, lens)| {
                (1 + box_nr) * (1 + lens_nr) * lens.focal_length as usize
            })
        })
        .sum::<usize>();

    submit!(2, focus_power);
}

fn hash(input: &str) -> u8 {
    input
        .as_bytes()
        .iter()
        .fold(0u16, |acc, c| ((acc + *c as u16) * 17) % 256) as u8
}

#[derive(Clone)]
struct Lens {
    label: &'static str,
    focal_length: u8,
}

enum Step {
    Remove(&'static str),
    Set(&'static str, u8),
}

impl Step {
    fn hash(&self) -> u8 {
        match self {
            Step::Remove(s) => hash(s),
            Step::Set(s, _) => hash(s),
        }
    }

    fn apply(&self, box_: &mut Vec<Lens>) {
        match *self {
            Step::Remove(s) => {
                box_.retain(|lens| lens.label != s);
            }
            Step::Set(label, focal_length) => {
                for lens in box_.iter_mut() {
                    if lens.label == label {
                        lens.focal_length = focal_length;
                        return;
                    }
                }
                box_.push(Lens {
                    label,
                    focal_length,
                });
            }
        }
    }
}

impl From<&'static str> for Step {
    fn from(input: &'static str) -> Self {
        if let Some(label) = input.strip_suffix('-') {
            Step::Remove(label)
        } else {
            let focal_length = input[input.len() - 1..].parse().unwrap();
            Step::Set(&input[..input.len() - 2], focal_length)
        }
    }
}
