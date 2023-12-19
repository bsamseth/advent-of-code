use aocd::prelude::*;
use itertools::Itertools;

#[aocd(2023, 13)]
fn main() {
    let input = input!();
    let images = input
        .split_terminator("\n\n")
        .map(Image::from)
        .collect_vec();

    submit!(1, images.iter().map(|im| im.checksum(0)).sum::<usize>());
    submit!(2, images.iter().map(|im| im.checksum(1)).sum::<usize>());
}

struct Image {
    data: Vec<Vec<char>>,
}

impl Image {
    fn checksum(&self, desired_diff: usize) -> usize {
        self.reflect_x(desired_diff).unwrap_or(0) * 100 + self.reflect_y(desired_diff).unwrap_or(0)
    }

    fn reflect_x(&self, desired_diff: usize) -> Option<usize> {
        (1..self.data.len()).find(|&i| {
            let diff: usize = (1..=i.min(self.data.len() - i))
                .map(|k| {
                    self.data[i - k]
                        .iter()
                        .zip(self.data[i + k - 1].iter())
                        .filter(|(a, b)| a != b)
                        .count()
                })
                .sum();

            diff == desired_diff
        })
    }

    fn reflect_y(&self, desired_diff: usize) -> Option<usize> {
        let transposed = (0..self.data[0].len())
            .map(|j| (0..self.data.len()).map(|i| self.data[i][j]).collect())
            .collect();
        let transposed = Image { data: transposed };
        transposed.reflect_x(desired_diff)
    }
}

impl From<&str> for Image {
    fn from(s: &str) -> Self {
        Self {
            data: s.lines().map(|l| l.chars().collect()).collect(),
        }
    }
}
