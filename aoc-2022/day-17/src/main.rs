use itertools::Itertools;
use std::{
    collections::{HashMap, VecDeque},
    fmt::Display,
};

use aocd::*;

// The main function isn't that bad. Just don't look at what comes after it. Spagehtti special.
#[aocd(2022, 17)]
fn main() {
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

    let mut chamber = Chamber::new(&jets, &pieces);
    let mut signs: HashMap<String, (u64, u64)> = HashMap::new();
    let mut period = 0u64;
    let mut i = 1u64;
    let target = 1_000_000_000_000u64;
    while i <= target {
        chamber.drop_piece();

        if i == 2022 {
            submit!(1, chamber.total_height());
        }

        if period == 0 && i >= 10_000 {
            if let Some((last_i, last_height)) =
                signs.insert(chamber.signature(), (i, chamber.total_height() as u64))
            {
                period = i - last_i;
                let height_diff = chamber.total_height() as u64 - last_height;
                assert!(last_i == 10_000);

                let skips = (target - i) / period;
                chamber.pruned_height += (height_diff * skips) as usize;
                i += skips * period;
            }
        }

        i += 1;
    }
    submit!(2, chamber.total_height());
}

#[derive(Debug, Clone)]
struct Piece(Vec<Vec<char>>);

#[derive(Debug)]
struct Chamber {
    rows: VecDeque<Vec<char>>,
    in_motion: usize,
    pruned_height: usize,
    jet_index: usize,
    piece_index: usize,
    jets: Vec<char>,
    pieces: Vec<Piece>,
}

impl Chamber {
    fn new(jets: &[char], pieces: &[Piece]) -> Self {
        let mut rows = VecDeque::new();
        rows.push_back(vec!['-'; 7]);
        Self {
            rows,
            in_motion: 0,
            pruned_height: 0,
            jet_index: 0,
            piece_index: 0,
            jets: jets.into(),
            pieces: pieces.into(),
        }
    }

    fn spawn(&mut self) {
        let piece = &self.pieces[self.piece_index % self.pieces.len()];
        let room = self.rows.len() - self.highest_piece() - 1;

        for _ in room..3 {
            self.rows.push_back(vec!['.'; 7])
        }
        for _ in 3..room {
            self.rows.pop_back();
        }

        self.rows.extend(piece.0.clone());
        self.in_motion = piece
            .0
            .iter()
            .map(|row| row.iter().filter(|&&c| c == '@').count())
            .sum();
    }

    fn prune(&mut self) {
        for _ in 0..(self.rows.len() as isize - 1000).max(0) {
            self.rows.pop_front();
            self.pruned_height += 1;
        }
    }

    fn drop_piece(&mut self) {
        self.prune();
        self.spawn();
        self.piece_index += 1;

        loop {
            self.jet(self.jets[self.jet_index % self.jets.len()]);
            self.jet_index += 1;

            if !self.drop() {
                break;
            }
        }
    }

    fn signature(&self) -> String {
        format!(
            "{} {} {}",
            self.jet_index % self.jets.len(),
            self.piece_index % self.pieces.len(),
            self.rows.iter().map(|r| r.iter().format("")).format("\n")
        )
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

    fn total_height(&self) -> usize {
        self.highest_piece() + self.pruned_height
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
