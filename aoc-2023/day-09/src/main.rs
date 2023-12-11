use aocd::prelude::*;

#[aocd(2023, 9)]
fn main() {
    let histories = input!()
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(str::parse)
                .collect::<Result<Vec<i64>, _>>()
                .unwrap()
        })
        .collect::<Vec<_>>();

    let (p2, p1) = histories
        .iter()
        .map(|h| extrapolate(h))
        .reduce(|(a, b), (c, d)| (a + c, b + d))
        .unwrap();

    submit!(1, p1);
    submit!(2, p2);
}

fn extrapolate(history: &[i64]) -> (i64, i64) {
    let diff: Vec<i64> = history
        .iter()
        .skip(1)
        .zip(history.iter())
        .map(|(a, b)| a - b)
        .collect();

    let (left, right) = if diff.iter().any(|&x| x != 0) {
        extrapolate(&diff)
    } else {
        (0, 0)
    };

    (
        history.first().unwrap() - left,
        history.last().unwrap() + right,
    )
}
