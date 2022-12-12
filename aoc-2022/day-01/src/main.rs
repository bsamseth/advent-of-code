use aocd::Aocd;

fn main() {
    let client = Aocd::new(2022, 1);
    let input = client.get_input();

    let mut elves: Vec<_> = input
        .split("\n\n")
        .map(|e| e.lines().map(|l| l.parse::<u32>().unwrap()).sum())
        .collect();
    elves.sort();

    client.submit(1, elves.last().unwrap());
    client.submit(2, elves.iter().rev().take(3).sum::<u32>());
}
