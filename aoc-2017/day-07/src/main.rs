use aocd::prelude::*;

#[derive(Debug)]
struct Program {
    name: &'static str,
    weight: u32,
    children: Vec<&'static str>,
}

impl Program {
    fn total_weight(&self, programs: &[Program]) -> u32 {
        let mut weight = self.weight;
        for child in &self.children {
            let child = programs.iter().find(|p| p.name == *child).unwrap();
            weight += child.total_weight(programs);
        }
        weight
    }

    fn weight_offset(&self, programs: &[Program]) -> u32 {
        if self.children.is_empty() {
            return 0;
        }

        let children = self
            .children
            .iter()
            .map(|child_name| {
                programs
                    .iter()
                    .find(|program| program.name == *child_name)
                    .unwrap()
            })
            .collect::<Vec<_>>();

        if let Some(diff) = children
            .iter()
            .map(|child| child.weight_offset(programs))
            .find(|&d| d != 0)
        {
            return diff;
        }

        let weights = children
            .iter()
            .map(|child| child.total_weight(programs))
            .collect::<Vec<_>>();

        let mut weights_with_counts = weights
            .iter()
            .enumerate()
            .map(|(i, &weight)| {
                (
                    children[i],
                    weight,
                    weights.iter().filter(|&&w| w == weight).count(),
                )
            })
            .collect::<Vec<_>>();

        weights_with_counts.sort_by_key(|&(_, _, count)| count);

        let (child, total_weight, count) = weights_with_counts[0];
        if count == 1 {
            ((weights_with_counts[1].1 as i64 - total_weight as i64) + child.weight as i64) as u32
        } else {
            0
        }
    }
}

impl From<&'static str> for Program {
    fn from(s: &'static str) -> Self {
        let mut parts = s.split(" -> ");
        let mut name_weight = parts.next().unwrap().split_whitespace();
        let name = name_weight.next().unwrap();
        let weight = name_weight.next().unwrap();
        let weight = weight[1..weight.len() - 1].parse().unwrap();

        let children = parts
            .next()
            .map(|s| s.split_terminator(", "))
            .map_or(vec![], Iterator::collect);
        Program {
            name,
            weight,
            children,
        }
    }
}

#[aocd(2017, 7)]
fn main() {
    // let input = std::fs::read_to_string("test.txt").unwrap();
    let programs = input!()
        .leak()
        .lines()
        .map(Program::from)
        .collect::<Vec<_>>();

    let root = programs
        .iter()
        .find(|program| !programs.iter().any(|p| p.children.contains(&program.name)))
        .unwrap();

    submit!(1, root.name);
    submit!(2, root.weight_offset(&programs));
}
