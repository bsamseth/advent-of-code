use aocd::*;
use std::collections::HashMap;

#[derive(Debug)]
enum Job {
    Direct(i64),
    Add(String, String),
    Multiply(String, String),
    Subtract(String, String),
    Divide(String, String),
    Eq(String, String),
}

fn evaluate(jobs: &HashMap<&str, Job>, name: &str) -> i64 {
    match jobs.get(name).unwrap() {
        Job::Direct(value) => *value,
        Job::Add(a, b) => evaluate(jobs, a) + evaluate(jobs, b),
        Job::Multiply(a, b) => evaluate(jobs, a) * evaluate(jobs, b),
        Job::Subtract(a, b) => evaluate(jobs, a) - evaluate(jobs, b),
        Job::Divide(a, b) => {
            let a = evaluate(jobs, a);
            let b = evaluate(jobs, b);
            a / b
        }
        Job::Eq(a, b) => {
            let a = evaluate(jobs, a);
            let b = evaluate(jobs, b);
            b - a
        }
    }
}

#[aocd(2022, 21)]
fn main() {
    let input = input!();
    let mut jobs = input
        .lines()
        .map(|l| {
            let mut x = l.split(": ");
            let id = x.next().unwrap();
            let y = x.next().unwrap().split(" ").collect::<Vec<_>>();
            let job = match y[..] {
                [value] => Job::Direct(value.parse().unwrap()),
                [left, op, right] => match op {
                    "+" => Job::Add(left.to_string(), right.to_string()),
                    "*" => Job::Multiply(left.to_string(), right.to_string()),
                    "-" => Job::Subtract(left.to_string(), right.to_string()),
                    "/" => Job::Divide(left.to_string(), right.to_string()),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            };
            (id, job)
        })
        .collect::<HashMap<_, _>>();

    submit!(1, evaluate(&jobs, "root"));

    jobs.insert(
        "root",
        match jobs.get("root").unwrap() {
            Job::Add(a, b) => Job::Eq(a.to_string(), b.to_string()),
            Job::Multiply(a, b) => Job::Eq(a.to_string(), b.to_string()),
            Job::Subtract(a, b) => Job::Eq(a.to_string(), b.to_string()),
            Job::Divide(a, b) => Job::Eq(a.to_string(), b.to_string()),
            _ => unreachable!(),
        },
    );

    let mut eval = |yell| {
        jobs.insert("humn", Job::Direct(yell));
        evaluate(&jobs, "root")
    };

    // Find a range to binary search over.
    let sign = eval(0).is_positive();
    let mut lower = 0;
    let mut upper = 0;
    for i in 0..63 {
        let x = 1i64 << i;
        if eval(x).is_positive() != sign {
            upper = x;
            break;
        }
        lower = x;
    }

    // Binary search for the correct value.
    while lower < upper {
        let mid = (lower + upper) / 2;
        let result = eval(mid);
        if result == 0 {
            // Found a zero, but need to check if somewhere else in the range an earlier one.
            for x in lower..upper {
                if eval(x) == 0 {
                    submit!(2, x);
                    return;
                }
            }
        } else if result < 0 {
            lower = mid + 1;
        } else {
            upper = mid - 1;
        }
    }
}
