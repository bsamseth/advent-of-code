use std::cmp::Ordering;

use aocd::prelude::*;

#[aocd(2024, 5)]
fn main() {
    let input = input!();
    let (rules, updates) = input.split_once("\n\n").unwrap();
    let rules: Vec<Rule> = rules
        .lines()
        .map(|line| {
            let (a, b) = line.split_once('|').unwrap();
            Rule {
                a: a.parse().unwrap(),
                b: b.parse().unwrap(),
            }
        })
        .collect();

    let cmp = |a, b| {
        for rule in &rules {
            if rule.a == a && rule.b == b {
                return Ordering::Less;
            }
            if rule.a == b && rule.b == a {
                return Ordering::Greater;
            }
        }
        Ordering::Equal
    };

    let updates: Vec<Vec<u32>> = updates
        .lines()
        .map(|line| line.split(',').map(|n| n.parse().unwrap()).collect())
        .collect();

    let updates_ordered: Vec<Vec<u32>> = updates
        .iter()
        .map(|update| {
            let mut update = update.clone();
            update.sort_by(|a, b| cmp(*a, *b));
            update
        })
        .collect();

    let mut p1 = 0;
    let mut p2 = 0;
    for (org, ord) in updates.iter().zip(&updates_ordered) {
        if org == ord {
            p1 += org[org.len() >> 1];
        } else {
            p2 += ord[ord.len() >> 1];
        }
    }

    submit!(1, p1);
    submit!(2, p2);
}

#[derive(Debug)]
struct Rule {
    a: u32,
    b: u32,
}
