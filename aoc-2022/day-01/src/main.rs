use aocd_rs::Aocd;

fn main() {
    let client = Aocd::new();
    let input = client.get_input(2022, 1);

    let mut elves: Vec<_> = input
        .split("\n\n")
        .map(|e| e.lines().map(|l| l.parse::<u32>().unwrap()).sum())
        .collect();
    elves.sort();

    client.submit(2022, 1, 1, elves.last().unwrap());
    client.submit(2022, 1, 2, elves.iter().rev().take(3).sum::<u32>());
}
