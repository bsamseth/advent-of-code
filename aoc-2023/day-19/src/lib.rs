use itertools::Itertools;

pub mod part1;
pub mod part2;

pub type Label = &'static str;

pub struct Workflow {
    pub name: Label,
    rules: Vec<Rule>,
}

#[derive(Clone)]
struct Rule {
    condition: Option<Condition>,
    outcome: WorkflowResult,
}

#[derive(Clone)]
struct Condition {
    category: u8,
    gt: bool,
    value: u32,
}

pub struct Part {
    x: u32,
    m: u32,
    a: u32,
    s: u32,
}

#[derive(Clone)]
enum WorkflowResult {
    Accept,
    Reject,
    Redirect(Label),
}

impl From<&'static str> for Workflow {
    fn from(s: &'static str) -> Self {
        let (name, rest) = s.split_once('{').unwrap();
        let rules = rest[..rest.len() - 1]
            .split_terminator(',')
            .map(Rule::from)
            .collect_vec();
        Self { name, rules }
    }
}

impl From<&'static str> for Rule {
    fn from(s: &'static str) -> Self {
        if let Some((condition, dest)) = s.split_once(':') {
            let category = condition.as_bytes()[0];
            let gt = condition.as_bytes()[1] == b'>';
            let value = condition[2..].parse::<u32>().unwrap();
            Rule {
                outcome: dest.into(),
                condition: Some(Condition {
                    category,
                    gt,
                    value,
                }),
            }
        } else {
            Rule {
                outcome: s.into(),
                condition: None,
            }
        }
    }
}

impl From<&'static str> for WorkflowResult {
    fn from(s: &'static str) -> Self {
        match s {
            "A" => Self::Accept,
            "R" => Self::Reject,
            _ => Self::Redirect(s),
        }
    }
}

impl From<&str> for Part {
    fn from(s: &str) -> Self {
        let mut parts = s[1..s.len() - 1]
            .split_terminator(',')
            .map(|s| s.split_once('=').unwrap().1.parse::<u32>().unwrap());
        Self {
            x: parts.next().unwrap(),
            m: parts.next().unwrap(),
            a: parts.next().unwrap(),
            s: parts.next().unwrap(),
        }
    }
}
