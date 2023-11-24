use aocd::prelude::*;

#[aocd(2017, 6)]
fn main() {
    let mut banks = input!()
        .split_whitespace()
        .map(str::parse)
        .collect::<Result<Vec<u32>, _>>()
        .unwrap();

    let mut seen = std::collections::HashMap::new();

    let mut count = 0;
    while !seen.contains_key(&banks.hash()) {
        seen.insert(banks.hash(), count);
        banks.reallocation();
        count += 1;
    }

    submit!(1, count);
    submit!(2, count - seen.get(&banks.hash()).unwrap())
}

// Why a trait? Because I can.
trait Banks {
    fn max_bank(&self) -> usize;
    fn reallocation(&mut self);
    fn hash(&self) -> String;
}

impl Banks for Vec<u32> {
    fn max_bank(&self) -> usize {
        let mut i = 0;
        let mut max = 0;
        for (j, value) in self.iter().enumerate() {
            if value > &max {
                i = j;
                max = *value;
            }
        }
        i
    }

    fn reallocation(&mut self) {
        let mut bank_index = self.max_bank();
        let to_reallocate = self[bank_index];
        self[bank_index] = 0;
        for _ in 0..to_reallocate {
            bank_index = (bank_index + 1) % self.len();
            self[bank_index] += 1;
        }
    }

    fn hash(&self) -> String {
        self.iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join(",")
    }
}
