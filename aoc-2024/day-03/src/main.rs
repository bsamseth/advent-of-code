use aocd::prelude::*;

#[aocd(2024, 3)]
fn main() {
    let input = input!();
    let mul_regex = regex::Regex::new(r"mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)").unwrap();
    let mut p1 = 0;
    let mut p2 = 0;
    let mut enabled = true;

    for m in mul_regex.find_iter(&input) {
        let m = m.as_str();
        match m {
            "do()" => enabled = true,
            "don't()" => enabled = false,
            x => {
                let mut nums = x
                    .trim_start_matches("mul(")
                    .trim_end_matches(')')
                    .split(',')
                    .map(|n| n.parse::<u64>().unwrap());

                let a = nums.next().unwrap();
                let b = nums.next().unwrap();

                p1 += a * b;
                if enabled {
                    p2 += a * b;
                }
            }
        }
    }
    submit!(1, p1);
    submit!(2, p2);
}
