use aocd::prelude::*;

#[derive(Debug)]
struct Card {
    winning: Vec<u32>,
    inventory: Vec<u32>,
}

impl Card {
    fn matches(&self) -> usize {
        self.inventory
            .iter()
            .filter(|n| self.winning.contains(n))
            .count()
    }
    fn points(&self) -> u64 {
        let matches = self.matches() as u32;
        if matches == 0 {
            0
        } else {
            2u64.pow(matches - 1)
        }
    }
}

#[aocd(2023, 4)]
fn main() {
    let cards: Vec<Card> = input!()
        .lines()
        .map(|l| {
            let numbers = l
                .split_whitespace()
                .map(|c| c.parse::<u32>())
                .filter_map(Result::ok)
                .collect::<Vec<_>>();

            let n_winning = l
                .split_whitespace()
                .enumerate()
                .find(|(_, c)| c.starts_with('|'))
                .unwrap()
                .0
                - 2;

            let (winning, inventory) = numbers.split_at(n_winning);
            Card {
                winning: winning.into(),
                inventory: inventory.into(),
            }
        })
        .collect();

    submit!(1, cards.iter().map(|c| c.points()).sum::<u64>());

    let mut stack = cards.iter().map(|c| (1, c)).collect::<Vec<_>>();
    for i in 0..stack.len() {
        for j in i + 1..=i + stack[i].1.matches() {
            stack[j].0 += stack[i].0;
        }
    }

    submit!(2, stack.iter().map(|(n, _)| n).sum::<u64>());
}
