use aocd::*;
use regex::Regex;

#[derive(Debug, Clone, Copy, PartialEq)]
enum Square {
    Empty,
    Open,
    Closed,
}

#[derive(Debug, Clone, Copy)]
enum Turn {
    Left,
    Right,
}

#[derive(Clone, Copy)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn turn(&self, turn: Turn) -> Direction {
        match turn {
            Turn::Left => match self {
                Direction::Up => Direction::Left,
                Direction::Down => Direction::Right,
                Direction::Left => Direction::Down,
                Direction::Right => Direction::Up,
            },
            Turn::Right => match self {
                Direction::Up => Direction::Right,
                Direction::Down => Direction::Left,
                Direction::Left => Direction::Up,
                Direction::Right => Direction::Down,
            },
        }
    }
}

#[derive(Debug)]
enum Instruction {
    Turn(Turn),
    Forward(usize),
}

#[derive(Debug, Clone, Copy)]
struct Coord {
    row: usize,
    col: usize,
}

fn act(
    map: &Vec<Vec<Square>>,
    mut pos: Coord,
    dir: Direction,
    inst: &Instruction,
) -> (Coord, Direction) {
    match inst {
        Instruction::Turn(turn) => (pos, dir.turn(*turn)),
        Instruction::Forward(0) => (pos, dir),
        Instruction::Forward(mut steps) => {
            let mut last_valid = pos;
            while steps > 0 {
                let new_pos = match dir {
                    Direction::Up => Coord {
                        row: (map.len() + pos.row - 1).rem_euclid(map.len()),
                        col: pos.col,
                    },
                    Direction::Down => Coord {
                        row: (pos.row + 1).rem_euclid(map.len()),
                        col: pos.col,
                    },
                    Direction::Left => Coord {
                        row: pos.row,
                        col: (map[pos.row].len() + pos.col - 1).rem_euclid(map[pos.row].len()),
                    },
                    Direction::Right => Coord {
                        row: pos.row,
                        col: (pos.col + 1).rem_euclid(map[pos.row].len()),
                    },
                };

                match map.get(new_pos.row).and_then(|r| r.get(new_pos.col)) {
                    None | Some(Square::Empty) => {
                        pos = new_pos;
                    }
                    Some(Square::Open) => {
                        pos = new_pos;
                        last_valid = pos;
                        steps -= 1
                    }
                    Some(Square::Closed) => break,
                }
            }
            (last_valid, dir)
        }
    }
}

#[aocd(2022, 22)]
fn main() {
    let input = input!();
    let (map, path) = match input.split("\n\n").collect::<Vec<_>>()[..] {
        [map, path] => (map, path),
        _ => panic!("Invalid input"),
    };

    let map = map
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| match c {
                    ' ' => Square::Empty,
                    '.' => Square::Open,
                    '#' => Square::Closed,
                    _ => panic!("Invalid map"),
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let path = Regex::new(r"(\d+|[LR])")
        .unwrap()
        .captures_iter(path)
        .map(|cap| match cap.get(1).unwrap().as_str() {
            "L" => Instruction::Turn(Turn::Left),
            "R" => Instruction::Turn(Turn::Right),
            num => Instruction::Forward(num.parse().unwrap()),
        })
        .collect::<Vec<_>>();

    let mut pos = Coord {
        row: 0,
        col: map[0]
            .iter()
            .enumerate()
            .filter(|(_, &c)| c == Square::Open)
            .map(|(i, _)| i)
            .next()
            .unwrap(),
    };
    let mut direction = Direction::Right;

    for inst in path.iter() {
        (pos, direction) = act(&map, pos, direction, inst);
    }

    submit!(
        1,
        (pos.row + 1) * 1000
            + 4 * (pos.col + 1)
            + match direction {
                Direction::Right => 0,
                Direction::Down => 1,
                Direction::Left => 2,
                Direction::Up => 3,
            }
    );
}
