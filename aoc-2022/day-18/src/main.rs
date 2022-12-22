use aocd::*;
use std::collections::HashSet;

type Point = (i32, i32, i32);

const fn neighbors(point: &Point) -> [Point; 6] {
    let &(x, y, z) = point;
    [
        (x - 1, y, z),
        (x + 1, y, z),
        (x, y - 1, z),
        (x, y + 1, z),
        (x, y, z - 1),
        (x, y, z + 1),
    ]
}

fn part1(droplets: &HashSet<Point>) -> usize {
    droplets
        .iter()
        .map(|d| {
            neighbors(d)
                .iter()
                .filter(|&n| !droplets.contains(n))
                .count()
        })
        .sum()
}

fn part2(droplets: &HashSet<Point>) -> usize {
    let min_x = droplets.iter().map(|(x, _, _)| x).min().unwrap() - 1;
    let max_x = droplets.iter().map(|(x, _, _)| x).max().unwrap() + 1;
    let min_y = droplets.iter().map(|(_, y, _)| y).min().unwrap() - 1;
    let max_y = droplets.iter().map(|(_, y, _)| y).max().unwrap() + 1;
    let min_z = droplets.iter().map(|(_, _, z)| z).min().unwrap() - 1;
    let max_z = droplets.iter().map(|(_, _, z)| z).max().unwrap() + 1;

    let mut visited = HashSet::new();
    let mut queue = vec![(min_x, min_y, min_z)];
    let mut count = 0;
    while let Some((x, y, z)) = queue.pop() {
        if x < min_x
            || x > max_x
            || y < min_y
            || y > max_y
            || z < min_z
            || z > max_z
            || visited.contains(&(x, y, z))
        {
            continue;
        }
        if droplets.contains(&(x, y, z)) {
            count += 1;
        } else {
            visited.insert((x, y, z));
            queue.extend(neighbors(&(x, y, z)));
        }
    }
    count
}

#[aocd(2022, 18)]
fn main() {
    let droplets = input!()
        .lines()
        .map(|l| {
            let coords = l
                .split(',')
                .map(|n| n.parse::<i32>().unwrap())
                .collect::<Vec<_>>();
            (coords[0], coords[1], coords[2])
        })
        .collect();

    submit!(1, part1(&droplets));
    submit!(2, part2(&droplets));
}
