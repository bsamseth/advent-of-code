use aocd::prelude::*;

type EnergyState = u8;
const ENERGY_UP: EnergyState = 0b0001;
const ENERGY_DOWN: EnergyState = 0b0010;
const ENERGY_LEFT: EnergyState = 0b0100;
const ENERGY_RIGHT: EnergyState = 0b1000;

#[aocd(2023, 16)]
fn main() {
    let grid = Grid::from(input!());

    submit!(1, propagate_energy(grid.clone(), 0, 0, ENERGY_RIGHT));

    let starts = (0..grid.0.len())
        .map(|y| (y, 0, ENERGY_RIGHT))
        .chain((0..grid.0.len()).map(|y| (y, grid.0[y].len() - 1, ENERGY_LEFT)))
        .chain((0..grid.0[0].len()).map(|x| (0, x, ENERGY_DOWN)))
        .chain((0..grid.0[0].len()).map(|x| (grid.0.len() - 1, x, ENERGY_UP)));

    submit!(
        2,
        starts
            .map(|(y, x, energy_state)| propagate_energy(grid.clone(), y, x, energy_state))
            .max()
            .unwrap()
    );
}

fn propagate_energy(mut grid: Grid, y: usize, x: usize, energy_direction: EnergyState) -> usize {
    let mut queue = vec![(y as isize, x as isize, energy_direction)];
    while let Some((y, x, energy_direction)) = queue.pop() {
        // Check if we are out of bounds, or if we've already seen this tile in this direction.
        if y < 0 || x < 0 || y as usize >= grid.0.len() || x as usize >= grid.0[y as usize].len() {
            continue;
        }
        let tile = &mut grid.0[y as usize][x as usize];

        if tile.energy_state & energy_direction != 0 {
            continue;
        }

        // Mark tile as visited in this direction.
        tile.energy_state |= energy_direction;

        // Propagate energy.
        match tile.kind {
            TileKind::Empty => match energy_direction {
                ENERGY_UP => queue.push((y - 1, x, ENERGY_UP)),
                ENERGY_DOWN => queue.push((y + 1, x, ENERGY_DOWN)),
                ENERGY_LEFT => queue.push((y, x - 1, ENERGY_LEFT)),
                ENERGY_RIGHT => queue.push((y, x + 1, ENERGY_RIGHT)),
                _ => unreachable!(),
            },
            TileKind::HorizontalSplitter => match energy_direction {
                ENERGY_LEFT => queue.push((y, x - 1, ENERGY_LEFT)),
                ENERGY_RIGHT => queue.push((y, x + 1, ENERGY_RIGHT)),
                ENERGY_UP | ENERGY_DOWN => {
                    queue.push((y, x - 1, ENERGY_LEFT));
                    queue.push((y, x + 1, ENERGY_RIGHT));
                }
                _ => unreachable!(),
            },
            TileKind::VerticalSplitter => match energy_direction {
                ENERGY_UP => queue.push((y - 1, x, ENERGY_UP)),
                ENERGY_DOWN => queue.push((y + 1, x, ENERGY_DOWN)),
                ENERGY_LEFT | ENERGY_RIGHT => {
                    queue.push((y - 1, x, ENERGY_UP));
                    queue.push((y + 1, x, ENERGY_DOWN));
                }
                _ => unreachable!(),
            },
            TileKind::ForwardMirror => match energy_direction {
                ENERGY_UP => queue.push((y, x + 1, ENERGY_RIGHT)),
                ENERGY_DOWN => queue.push((y, x - 1, ENERGY_LEFT)),
                ENERGY_LEFT => queue.push((y + 1, x, ENERGY_DOWN)),
                ENERGY_RIGHT => queue.push((y - 1, x, ENERGY_UP)),
                _ => unreachable!(),
            },
            TileKind::BackwardMirror => match energy_direction {
                ENERGY_UP => queue.push((y, x - 1, ENERGY_LEFT)),
                ENERGY_DOWN => queue.push((y, x + 1, ENERGY_RIGHT)),
                ENERGY_LEFT => queue.push((y - 1, x, ENERGY_UP)),
                ENERGY_RIGHT => queue.push((y + 1, x, ENERGY_DOWN)),
                _ => unreachable!(),
            },
        }
    }

    grid.0
        .iter()
        .flatten()
        .filter(|t| t.energy_state != 0)
        .count()
}

#[derive(Clone)]
enum TileKind {
    Empty,
    HorizontalSplitter,
    VerticalSplitter,
    ForwardMirror,
    BackwardMirror,
}
#[derive(Clone)]
struct Tile {
    kind: TileKind,
    energy_state: EnergyState,
}

#[derive(Clone)]
struct Grid(Vec<Vec<Tile>>);

impl From<String> for Grid {
    fn from(s: String) -> Self {
        Grid(
            s.lines()
                .map(|l| l.chars().map(Tile::from).collect())
                .collect(),
        )
    }
}

impl From<char> for Tile {
    fn from(c: char) -> Self {
        let kind = match c {
            '.' => TileKind::Empty,
            '-' => TileKind::HorizontalSplitter,
            '|' => TileKind::VerticalSplitter,
            '/' => TileKind::ForwardMirror,
            '\\' => TileKind::BackwardMirror,
            _ => panic!("Invalid character: {}", c),
        };

        Tile {
            kind,
            energy_state: 0,
        }
    }
}

impl std::fmt::Display for Grid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for row in &self.0 {
            for tile in row {
                let c = match tile.kind {
                    TileKind::Empty => '.',
                    TileKind::HorizontalSplitter => '-',
                    TileKind::VerticalSplitter => '|',
                    TileKind::ForwardMirror => '/',
                    TileKind::BackwardMirror => '\\',
                };

                match tile.energy_state {
                    0 => write!(f, "{}", c)?,
                    _ => write!(f, "\x1B[31m{}\x1B[0m", c)?,
                }
            }
            writeln!(f)?;
        }
        Ok(())
    }
}
