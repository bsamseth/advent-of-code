use aocd::*;
use regex::Regex;

#[derive(Debug, Clone)]
enum Instruction {
    Left,
    Right,
    Forward(u32),
}

fn walk(
    map: &Vec<Vec<char>>,
    y: i32,
    x: i32,
    direction: (i32, i32),
    inst: Instruction,
) -> (i32, i32, (i32, i32)) {
    match inst {
        Instruction::Left => (
            y,
            x,
            match direction {
                (0, 1) => (0 - 1, 0),
                (0, -1) => (1, 0),
                (1, 0) => (0, 1),
                (-1, 0) => (0, 0 - 1),
                _ => unreachable!(),
            },
        ),
        Instruction::Right => (
            y,
            x,
            match direction {
                (0, 1) => (1, 0),
                (0, -1) => (0 - 1, 0),
                (1, 0) => (0, 0 - 1),
                (-1, 0) => (0, 1),
                _ => unreachable!(),
            },
        ),
        Instruction::Forward(0) => (y, x, direction),
        Instruction::Forward(n) => {
            let (mut u, mut v) = (y, x);
            loop {
                u = (u + direction.0).rem_euclid(map.len() as i32);
                v = (v + direction.1).rem_euclid(map.get(u as usize).unwrap().len() as i32);
                if let Some(' ') = map.get(u as usize).and_then(|r| r.get(v as usize)) {
                    continue;
                }
                break;
            }
            match map[u as usize][v as usize] {
                '.' => walk(map, u, v, direction, Instruction::Forward(n - 1)),
                '#' => walk(map, y, x, direction, Instruction::Forward(0)),
                _ => unreachable!(),
            }
        }
    }
}

fn display(map: &Vec<Vec<char>>, y: i32, x: i32, direction: (i32, i32)) {
    let mut v = map.clone();
    v[y as usize][x as usize] = match direction {
        (0, 1) => '>',
        (0, -1) => '<',
        (1, 0) => 'v',
        (-1, 0) => '^',
        _ => unreachable!(),
    };
    for r in v {
        println!("{}", r.iter().collect::<String>());
    }
    println!();
}

#[aocd(2022, 22)]
fn main() {
    let input = input!();
    // let input = std::fs::read_to_string("input.txt").unwrap();
    //     let input = "        ...#
    //         .#..
    //         #...
    //         ....
    // ...#.......#
    // ........#...
    // ..#....#....
    // ..........#.
    //         ...#....
    //         .....#..
    //         .#......
    //         ......#.
    //
    // 10R5L5R10L4R5L5";
    let (map, path) = match input.split("\n\n").collect::<Vec<_>>()[..] {
        [map, path] => (map, path),
        _ => panic!("Invalid input"),
    };

    let map = map
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let path = Regex::new(r"(\d+|[LR])")
        .unwrap()
        .captures_iter(path)
        .map(|cap| match cap.get(1).unwrap().as_str() {
            "L" => Instruction::Left,
            "R" => Instruction::Right,
            num => Instruction::Forward(num.parse().unwrap()),
        })
        .collect::<Vec<_>>();

    let mut y = 0;
    let mut x = map[0]
        .iter()
        .enumerate()
        .filter(|(_, &c)| c == '.')
        .map(|(i, _)| i)
        .next()
        .unwrap() as i32;
    let mut direction = (0, 1);

    display(&map, y, x, direction);
    for inst in path.iter() {
        // println!("{:?}", inst);
        (y, x, direction) = walk(&map, y, x, direction, inst.clone());
        // display(&map, y, x, direction);
    }
    // println!("{:?}", path);

    println!(
        "{}",
        (y + 1) * 1000
            + 4 * (x + 1)
            + match direction {
                (0, 1) => 0,
                (0, -1) => 1,
                (1, 0) => 2,
                (-1, 0) => 3,
                _ => unreachable!(),
            }
    );
}
