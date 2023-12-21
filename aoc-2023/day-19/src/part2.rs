use std::collections::HashMap;

use crate::{Condition, Label, Workflow, WorkflowResult};

pub fn solve(workflows: &HashMap<Label, Workflow>) -> u64 {
    let mut queue = vec![("in", PartRange::new())];

    let mut combinations = 0;
    while let Some((label, mut part)) = queue.pop() {
        let workflow = workflows.get(label).unwrap();
        for rule in &workflow.rules {
            let (yes, no) = if let Some(condition) = &rule.condition {
                part.split(condition)
            } else {
                (Some(part.clone()), None)
            };

            match rule.outcome {
                WorkflowResult::Accept if yes.is_some() => {
                    combinations += yes.unwrap().combinations();
                }
                WorkflowResult::Redirect(label) if yes.is_some() => {
                    queue.push((label, yes.unwrap()));
                }
                _ => {}
            }

            if let Some(no) = no {
                part = no;
            } else {
                break;
            }
        }
    }

    combinations
}

#[derive(Clone)]
struct PartRange {
    x: Range,
    m: Range,
    a: Range,
    s: Range,
}

impl PartRange {
    fn new() -> Self {
        Self {
            x: Range::new(),
            m: Range::new(),
            a: Range::new(),
            s: Range::new(),
        }
    }

    fn combinations(&self) -> u64 {
        (self.x.max - self.x.min + 1) as u64
            * (self.m.max - self.m.min + 1) as u64
            * (self.a.max - self.a.min + 1) as u64
            * (self.s.max - self.s.min + 1) as u64
    }

    fn split(&self, condition: &Condition) -> (Option<Self>, Option<Self>) {
        let mut yes = self.clone();
        let mut no = self.clone();

        let (yes_value, no_value) = match condition.category {
            b'x' => (&mut yes.x, &mut no.x),
            b'm' => (&mut yes.m, &mut no.m),
            b'a' => (&mut yes.a, &mut no.a),
            b's' => (&mut yes.s, &mut no.s),
            _ => unreachable!(),
        };

        if condition.gt {
            yes_value.set_lower_bound(condition.value + 1);
            no_value.set_upper_bound(condition.value);
        } else {
            yes_value.set_upper_bound(condition.value - 1);
            no_value.set_lower_bound(condition.value);
        }

        (yes.ok(), no.ok())
    }

    fn ok(self) -> Option<Self> {
        if self.x.is_empty() || self.m.is_empty() || self.a.is_empty() || self.s.is_empty() {
            None
        } else {
            Some(self)
        }
    }
}

#[derive(Clone)]
struct Range {
    min: u32,
    max: u32,
}

impl Range {
    fn new() -> Self {
        Self { min: 1, max: 4000 }
    }

    fn set_lower_bound(&mut self, value: u32) {
        self.min = self.min.max(value);
    }

    fn set_upper_bound(&mut self, value: u32) {
        self.max = self.max.min(value);
    }

    fn is_empty(&self) -> bool {
        self.min > self.max
    }
}
