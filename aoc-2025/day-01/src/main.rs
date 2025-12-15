use aocd::prelude::*;

#[aocd(2025, 1)]
fn main() {
    let mut zero_count_p1 = 0;
    let mut zero_count_p2 = 0;
    let mut pos = 50;
    for line in input!().split_whitespace() {
        let (dir, n) = line.split_at(1);
        let n: i64 = n.parse().unwrap();
        let dir = if dir == "L" { -1 } else { 1 };

        for _ in 0..n {
            pos = (pos + dir) % 100;
            if pos == 0 {
                zero_count_p2 += 1;
            }
        }
        if pos == 0 {
            zero_count_p1 += 1;
        }
    }
    submit!(1, zero_count_p1);
    submit!(2, zero_count_p2);
}
