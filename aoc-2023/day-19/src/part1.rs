use std::collections::HashMap;

use crate::{Label, Part, Workflow, WorkflowResult};

pub fn solve(workflows: &HashMap<Label, Workflow>, parts: &[Part]) -> u32 {
    parts
        .iter()
        .map(|part| {
            if let WorkflowResult::Accept = workflows.process(part) {
                part.x + part.m + part.a + part.s
            } else {
                0
            }
        })
        .sum::<u32>()
}

trait Process {
    fn process(&self, part: &Part) -> WorkflowResult;
}

impl Process for HashMap<Label, Workflow> {
    fn process(&self, part: &Part) -> WorkflowResult {
        let mut result = WorkflowResult::Redirect("in");
        while let WorkflowResult::Redirect(label) = result {
            let workflow = self.get(label).unwrap();
            result = workflow
                .rules
                .iter()
                .find_map(|rule| {
                    if let Some(condition) = &rule.condition {
                        let value = match condition.category {
                            b'x' => part.x,
                            b'm' => part.m,
                            b'a' => part.a,
                            b's' => part.s,
                            _ => unreachable!(),
                        };
                        if (condition.gt && value > condition.value)
                            || (!condition.gt && value < condition.value)
                        {
                            Some(rule.outcome.clone())
                        } else {
                            None
                        }
                    } else {
                        Some(rule.outcome.clone())
                    }
                })
                .unwrap();
        }
        result
    }
}
