use aocd::prelude::*;

#[aocd(2024, 7)]
fn main() {
    let mut p1 = 0;
    let mut p2 = 0;
    for line in input!().lines() {
        let (target, rest) = line.split_once(": ").unwrap();
        let target = target.parse::<i64>().unwrap();
        let rest = rest
            .split_whitespace()
            .map(|x| x.parse::<i64>().unwrap())
            .collect::<Vec<_>>();
        if check::<false>(target, 0, &rest) {
            p1 += target;
            p2 += target;
        } else if check::<true>(target, 0, &rest) {
            p2 += target;
        }
    }
    submit!(1, p1);
    submit!(2, p2);
}

fn check<const CONCAT: bool>(target: i64, cur: i64, rest: &[i64]) -> bool {
    if rest.is_empty() {
        return target == cur;
    }
    let x = rest[0];
    check::<CONCAT>(target, cur + x, &rest[1..])
        || check::<CONCAT>(target, cur * x, &rest[1..])
        || (CONCAT && check::<CONCAT>(target, concat(cur, x), &rest[1..]))
}

const fn concat(mut x: i64, y: i64) -> i64 {
    let mut a = y;
    while a > 0 {
        x *= 10;
        a /= 10;
    }
    x + y
}
