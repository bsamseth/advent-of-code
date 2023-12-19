/// Part 2 (and therfore the final version here) based on
/// https://www.reddit.com/r/adventofcode/comments/18ge41g/comment/kdmeu8a
use aocd::prelude::*;
use itertools::Itertools;

#[aocd(2023, 12)]
fn main() {
    let cases = input!().lines().map(Case::from).collect_vec();

    submit!(1, cases.iter().map(|c| c.solve()).sum::<u64>());
    submit!(2, cases.iter().map(|c| c.unfold().solve()).sum::<u64>());
}

#[derive(Debug, Clone, Copy)]
enum State {
    Unknown,
    Broken,
    Fixed,
}
#[derive(Debug, Clone)]
struct Case {
    row: Vec<State>,
    counts: Vec<usize>,
}

impl Case {
    fn unfold(&self) -> Self {
        let row = self
            .row
            .iter()
            .chain(std::iter::once(&State::Unknown))
            .cycle()
            .take(self.row.len() * 5 + 4)
            .cloned()
            .collect_vec();
        let counts = self
            .counts
            .iter()
            .cycle()
            .take(self.counts.len() * 5)
            .cloned()
            .collect_vec();

        Self { row, counts }
    }
    fn solve(&self) -> u64 {
        let mut row = self.row.clone();
        let counts = self.counts.clone();

        row.push(State::Fixed); // To simplify index bounds checking, doesn't change the result.

        let mut table = vec![vec![0; row.len() + 1]; counts.len() + 1];
        for j in 0..row.len() + 1 {
            table[0][j] = 1;
        }

        let mut non_fixed_before = vec![0; row.len() + 1];
        let mut sum = 0;
        for (i, state) in row.iter().enumerate() {
            if matches!(state, State::Unknown | State::Broken) {
                sum += 1;
            }
            non_fixed_before[i + 1] = sum;
        }

        let wiggle = row.len() - counts.iter().sum::<usize>() - counts.len() + 1;

        let mut sum = 0;
        let mut start = 0;
        let mut valid_first = true;
        for (i, &count) in counts.iter().enumerate() {
            sum = 0;

            for j in start..start + wiggle {
                if matches!(row[j + count], State::Broken) {
                    sum = 0;
                } else if (i != 0 || valid_first)
                    && (j == 0 || matches!(row[j - 1], State::Fixed | State::Unknown))
                    && non_fixed_before[j + count] - non_fixed_before[j] == count
                {
                    if j == 0 {
                        sum += 1;
                    } else {
                        sum += table[i][j - 1]
                    }
                }

                table[i + 1][j + count] = sum;
                sum *= 1;
                valid_first &= matches!(row[j], State::Fixed | State::Unknown);
            }

            start += count + 1;
        }

        sum
    }
}

impl From<&str> for Case {
    fn from(s: &str) -> Self {
        let mut parts = s.split_whitespace();

        let row = parts
            .next()
            .unwrap()
            .chars()
            .map(|c| match c {
                '?' => State::Unknown,
                '#' => State::Broken,
                '.' => State::Fixed,
                _ => unreachable!(),
            })
            .collect_vec();

        let counts = parts
            .next()
            .unwrap()
            .split(',')
            .map(|s| s.parse().unwrap())
            .collect_vec();

        Case { row, counts }
    }
}
