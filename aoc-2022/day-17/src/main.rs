use std::fmt::Display;

use aocd::*;

#[derive(Debug)]
struct Piece(Vec<Vec<char>>);

#[derive(Debug)]
struct Chamber {
    rows: Vec<Vec<char>>,
    in_motion: usize,
}

impl Chamber {
    fn new() -> Self {
        Self {
            rows: vec![vec!['-'; 7]],
            in_motion: 0,
        }
    }

    fn spawn(&mut self, piece: &Piece) {
        let room = self.rows.len() - self.highest_piece() - 1;

        for _ in room..3 {
            self.rows.push(vec!['.'; 7])
        }
        for _ in 3..room {
            self.rows.pop();
        }

        self.rows.extend(piece.0.clone());
        self.in_motion = piece
            .0
            .iter()
            .map(|row| row.iter().filter(|&&c| c == '@').count())
            .sum();
    }

    fn can_move(&self, dx: isize) -> bool {
        let mut piece_count = 0;
        for (_i, row) in self.rows.iter().enumerate().rev() {
            for (j, c) in row.iter().enumerate() {
                if *c == '@' {
                    let x = j as isize + dx;
                    if x < 0 || x >= row.len() as isize {
                        return false;
                    }
                    if let Some('#') = row.get(x as usize) {
                        return false;
                    }

                    piece_count += 1;
                    if piece_count == self.in_motion {
                        return true;
                    }
                }
            }
        }
        true
    }

    fn can_drop(&self) -> bool {
        let mut piece_count = 0;
        for (_i, row) in self.rows.iter().enumerate().rev() {
            for (j, c) in row.iter().enumerate() {
                if *c == '@' {
                    let below = self.rows.get(_i - 1).and_then(|r| r.get(j)).unwrap_or(&'#');
                    if *below == '#' || *below == '-' {
                        return false;
                    }
                    piece_count += 1;
                    if piece_count == self.in_motion {
                        return true;
                    }
                }
            }
        }
        true
    }

    fn highest_piece(&self) -> usize {
        self.rows
            .iter()
            .enumerate()
            .rev()
            .filter(|(_, r)| r.iter().any(|&c| c != '.'))
            .map(|(i, _)| i)
            .next()
            .unwrap()
    }

    fn piece_lowest_row(&self) -> usize {
        let mut piece_started = false;
        let mut piece_lowest_row = self.rows.len();
        for (i, row) in self.rows.iter().enumerate().rev() {
            if !piece_started && row.iter().any(|&c| c == '@') {
                piece_started = true;
            } else if piece_started && !row.iter().any(|&c| c == '@') {
                piece_lowest_row = i + 1;
                break;
            }
        }
        piece_lowest_row
    }

    fn drop(&mut self) -> bool {
        if !self.can_drop() {
            for i in self.piece_lowest_row()..=self.highest_piece() {
                for j in 0..7 {
                    if self.rows[i][j] == '@' {
                        self.rows[i][j] = '#';
                    }
                }
            }
            return false;
        }

        let mut piece_count = 0;
        let from = self.piece_lowest_row();
        let to = self.highest_piece();
        for i in from..=to {
            for j in 0..7 {
                if self.rows[i][j] == '@' {
                    self.rows[i - 1][j] = '@';
                    self.rows[i][j] = self.rows.get(i + 1).map(|row| row[j]).unwrap_or('.');

                    piece_count += 1;
                    if piece_count == self.in_motion {
                        return true;
                    }
                }
            }
        }
        unreachable!();
    }

    fn jet(&mut self, jet: char) {
        let jet = if jet == '<' { -1 } else { 1 };
        if !self.can_move(jet) {
            return;
        }
        let mut piece_count = 0;
        for row in self.rows.iter_mut().rev() {
            let order: Vec<usize> = if jet == 1 {
                (0..row.len()).rev().collect()
            } else {
                (0..row.len()).collect()
            };
            for j in order {
                if row[j] == '@' {
                    let x = j as isize + jet;
                    if x < 0 || x >= row.len() as isize {
                        continue;
                    }
                    row[x as usize] = '@';
                    let y = j as isize - jet;
                    if y >= 0 {
                        let y = y as usize;
                        if let Some('@') = row.get(y) {
                            row[j] = '@';
                        } else {
                            row[j] = '.';
                        }
                    } else {
                        row[j] = '.';
                    }

                    piece_count += 1;
                    if piece_count == self.in_motion {
                        return;
                    }
                }
            }
        }
    }
}

impl Display for Chamber {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        for (i, row) in self.rows.iter().rev().enumerate() {
            let edge = if i == self.rows.len() - 1 { "+" } else { "|" };
            write!(fmt, "{}", edge)?;
            for cell in row {
                write!(fmt, "{}", cell)?;
            }
            writeln!(fmt, "{}", edge)?;
        }
        Ok(())
    }
}

#[aocd(2022, 17)]
fn main() {
    // let input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>";
    let input = input!();

    let jets = input.chars().collect::<Vec<_>>();
    let pieces = [
        "..@@@@.",
        "...@...\n..@@@..\n...@...",
        "..@@@..\n....@..\n....@..",
        "..@....\n..@....\n..@....\n..@....",
        "..@@...\n..@@...",
    ]
    .iter()
    .map(|p: &&str| Piece(p.lines().map(|l| l.chars().collect()).collect()))
    .collect::<Vec<_>>();

    let mut chamber = Chamber::new();

    let mut jet_index = 0;
    for (_i, piece) in (1..=2022).zip(pieces.iter().cycle()) {
        chamber.spawn(piece);

        loop {
            chamber.jet(jets[jet_index % jets.len()]);
            jet_index += 1;

            if !chamber.drop() {
                break;
            }
        }
    }

    submit!(1, chamber.highest_piece());
}
