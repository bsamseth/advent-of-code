use aocd::*;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point(i32, i32);

impl Point {
    fn towards(&self, other: Self) -> Self {
        Self(
            self.0 + (other.0 - self.0).signum(),
            self.1 + (other.1 - self.1).signum(),
        )
    }
}

struct Cave {
    cells: HashMap<Point, char>,
    floor: Option<i32>,
}

impl Cave {
    fn new(input: &str, floor: bool) -> Self {
        let mut cells = HashMap::new();
        cells.insert(Point(500, 0), '+');
        for line in input.lines() {
            let points = line.split(" -> ").map(|p| {
                let mut parts = p.split(',');
                let x = parts.next().unwrap().parse().unwrap();
                let y = parts.next().unwrap().parse().unwrap();
                Point(x, y)
            });
            points.clone().zip(points.skip(1)).for_each(|(mut a, b)| {
                while a != b {
                    cells.insert(a, '#');
                    a = a.towards(b);
                }
                cells.insert(b, '#');
            });
        }
        let floor = if floor {
            cells.keys().map(|p| p.1 + 2).max()
        } else {
            None
        };
        Self { cells, floor }
    }

    fn occupied(&self, p: &Point) -> bool {
        if let Some(floor) = self.floor {
            if p.1 == floor {
                return true;
            }
        }
        self.cells.contains_key(p)
    }

    fn fill(&mut self) {
        let max_y = self
            .floor
            .unwrap_or_else(|| self.cells.keys().map(|p| p.1).max().unwrap());
        loop {
            let mut sand = Point(500, 0);
            while sand.1 < max_y {
                let under = self.occupied(&Point(sand.0, sand.1 + 1));
                let under_left = self.occupied(&Point(sand.0 - 1, sand.1 + 1));
                let under_right = self.occupied(&Point(sand.0 + 1, sand.1 + 1));

                if !under {
                    sand.1 += 1
                } else if !under_left {
                    sand.1 += 1;
                    sand.0 -= 1;
                } else if !under_right {
                    sand.1 += 1;
                    sand.0 += 1;
                } else {
                    self.cells.insert(sand, 'O');
                    break;
                }
            }
            if sand.1 >= max_y || sand == Point(500, 0) {
                break;
            }
        }
    }

    fn count(&self) -> usize {
        self.cells.values().filter(|&&c| c == 'O').count()
    }
}

#[aocd(2022, 14)]
fn main() {
    let mut cave_without_floor = Cave::new(&input!(), false);
    let mut cave_with_floor = Cave::new(&input!(), true);
    cave_without_floor.fill();
    submit!(1, cave_without_floor.count());

    cave_with_floor.fill();
    submit!(2, cave_with_floor.count());
}
