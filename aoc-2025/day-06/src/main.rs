use aocd::prelude::*;

#[aocd(2025, 6)]
fn main() {
    let input = input!();

    let problems_part1 = parse_part1(&input);
    let problems_part2 = parse_part2(&input);
    let grand_total_p1: u64 = problems_part1.iter().map(Problem::compute).sum();
    let grand_total_p2: u64 = problems_part2.iter().map(Problem::compute).sum();

    submit!(1, grand_total_p1);
    submit!(2, grand_total_p2);
}

fn parse_part1(input: &str) -> Vec<Problem> {
    let mut numbers =
        vec![Vec::new(); input.split('\n').next().unwrap().split_whitespace().count()];
    let mut problems = Vec::new();
    for line in input.split('\n') {
        if line.starts_with(['+', '*']) {
            for (i, op) in line.split_whitespace().enumerate() {
                problems.push(Problem {
                    numbers: numbers[i].clone(),
                    op: if op == "+" {
                        Operation::Add
                    } else {
                        Operation::Mul
                    },
                });
            }
        } else {
            for (i, n) in line.split_whitespace().enumerate() {
                let n = n.parse().unwrap();
                numbers[i].push(n);
            }
        }
    }
    problems
}
fn parse_part2(input: &str) -> Vec<Problem> {
    let mut transposed = transpose(
        input
            .split('\n')
            .map(|line| line.as_bytes().to_vec())
            .collect::<Vec<_>>(),
    );
    transposed.reverse();

    let mut problems = Vec::new();
    let mut problem = Problem {
        numbers: Vec::new(),
        op: Operation::Add,
    };
    for column in transposed {
        if column.iter().all(|b| *b == b' ') {
            problems.push(problem);
            problem = Problem {
                numbers: Vec::new(),
                op: Operation::Add,
            };
            continue;
        }

        let n = column
            .iter()
            .filter(|b| b.is_ascii_digit())
            .map(|b| u64::from(*b - b'0'))
            .reduce(|acc, x| acc * 10 + x)
            .unwrap();
        problem.numbers.push(n);

        if let Some(x) = column.iter().find(|b| **b == b'+' || **b == b'*')
            && *x == b'*'
        {
            problem.op = Operation::Mul;
        }
    }
    problems.push(problem);

    problems
}

fn transpose(v: Vec<Vec<u8>>) -> Vec<Vec<u8>> {
    assert!(!v.is_empty());
    let len = v[0].len();
    let mut iters: Vec<_> = v
        .into_iter()
        .map(std::iter::IntoIterator::into_iter)
        .collect();
    (0..len)
        .map(|_| {
            iters
                .iter_mut()
                .map(|n| n.next().unwrap())
                .collect::<Vec<u8>>()
        })
        .collect()
}

#[derive(Debug)]
struct Problem {
    numbers: Vec<u64>,
    op: Operation,
}

impl Problem {
    fn compute(&self) -> u64 {
        self.numbers
            .iter()
            .copied()
            .reduce(|acc, e| match self.op {
                Operation::Add => acc + e,
                Operation::Mul => acc * e,
            })
            .unwrap()
    }
}

#[derive(Debug)]
enum Operation {
    Add,
    Mul,
}
