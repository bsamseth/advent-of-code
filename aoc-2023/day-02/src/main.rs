use aocd::prelude::*;
use std::{cmp::max, str::FromStr};

#[aocd(2023, 2)]
fn main() {
    let input = input!();
    let games: Vec<Game> = input
        .lines()
        .map(str::parse)
        .collect::<Result<Vec<Game>, _>>()
        .unwrap();

    let part1: u64 = games
        .iter()
        .filter(|g| g.draws.iter().flatten().all(|d| d.possible(14, 13, 12)))
        .map(|g| g.id)
        .sum();

    submit!(1, part1);

    let part2: u64 = games
        .iter()
        .map(|g| {
            let mut blues = 0;
            let mut greens = 0;
            let mut reds = 0;
            g.draws.iter().flatten().for_each(|d| match d.color {
                Color::Blue => blues = max(blues, d.count),
                Color::Green => greens = max(greens, d.count),
                Color::Red => reds = max(reds, d.count),
            });
            blues * greens * reds
        })
        .sum();

    submit!(2, part2);
}

#[derive(Debug)]
struct Game {
    id: u64,
    draws: Vec<Vec<Draw>>,
}
#[derive(Debug)]
struct Draw {
    color: Color,
    count: u64,
}

#[derive(Debug)]
enum Color {
    Blue,
    Green,
    Red,
}

impl FromStr for Game {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut split = s.split(": ");
        let id = split.next().unwrap()[5..].parse().unwrap();
        let draws = split
            .next()
            .unwrap()
            .split("; ")
            .map(|s| s.split(", ").map(|d| d.parse().unwrap()).collect())
            .collect();
        Ok(Game { id, draws })
    }
}

impl FromStr for Draw {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut split = s.split(' ');
        let count = split.next().unwrap().parse().unwrap();
        let color = split.next().unwrap().parse().unwrap();
        Ok(Draw { color, count })
    }
}

impl Draw {
    fn possible(&self, blues: u64, greens: u64, reds: u64) -> bool {
        match self.color {
            Color::Blue => self.count <= blues,
            Color::Green => self.count <= greens,
            Color::Red => self.count <= reds,
        }
    }
}

impl FromStr for Color {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "blue" => Ok(Color::Blue),
            "green" => Ok(Color::Green),
            "red" => Ok(Color::Red),
            _ => Err(()),
        }
    }
}
