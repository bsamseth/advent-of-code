use aocd::prelude::*;

#[derive(Debug)]
struct Node {
    label: String,
    left: *mut Node,
    right: *mut Node,
}

#[aocd(2023, 8)]
fn main() {
    let input = input!();
    let mut input = input.lines();
    let directions = input.next().unwrap().chars().collect::<Vec<_>>();

    let nodes_raw = input
        .skip(1)
        .map(|line| {
            let label = line.chars().take(3).collect::<String>();
            let left = line.chars().skip(7).take(3).collect::<String>();
            let right = line.chars().skip(12).take(3).collect::<String>();
            (label, left, right)
        })
        .collect::<Vec<_>>();

    let mut nodes = nodes_raw
        .iter()
        .map(|(label, _, _)| Node {
            label: label.clone(),
            left: std::ptr::null_mut::<Node>(),
            right: std::ptr::null_mut::<Node>(),
        })
        .collect::<Vec<_>>();

    for i in 0..nodes.len() {
        let (_, left, right) = &nodes_raw[i];
        nodes[i].left =
            nodes.iter().find(|n| n.label == *left).unwrap() as *const Node as *mut Node;
        nodes[i].right =
            nodes.iter().find(|n| n.label == *right).unwrap() as *const Node as *mut Node;
    }

    let mut steps = 0;
    let mut node = nodes.iter().find(|n| n.label == "AAA").unwrap();
    while node.label != "ZZZ" {
        node = unsafe {
            if directions[steps % directions.len()] == 'L' {
                node.left
            } else {
                node.right
            }
            .as_ref()
            .unwrap()
        };
        steps += 1;
    }

    submit!(1, steps);

    let mut at_nodes = nodes
        .iter()
        .filter(|n| n.label.ends_with('A'))
        .collect::<Vec<_>>();
    let mut all_steps = Vec::with_capacity(at_nodes.len());
    for node in at_nodes.iter_mut() {
        let mut steps = 0;
        while !node.label.ends_with('Z') {
            *node = unsafe {
                if directions[steps % directions.len()] == 'L' {
                    node.left
                } else {
                    node.right
                }
                .as_ref()
                .unwrap()
            };
            steps += 1;
        }
        all_steps.push(steps);
    }

    let mut lcm = all_steps[0];
    for j in all_steps.iter().skip(1) {
        lcm = num::integer::lcm(lcm, *j);
    }

    submit!(2, lcm);
}
