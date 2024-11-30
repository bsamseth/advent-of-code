use std::collections::{HashMap, HashSet};

use aocd::prelude::*;
use itertools::Itertools;

#[aocd(2023, 23)]
fn main() {
    let mut chars = input!()
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    submit!(1, solve(&chars));

    // Make all `<^v>` behave like `.`:
    for line in &mut chars {
        for c in line {
            if *c != '#' {
                *c = '.';
            }
        }
    }
    submit!(2, solve(&chars));
}

type Graph = HashMap<(usize, usize), Vec<(usize, usize, u64)>>;

fn solve(chars: &[Vec<char>]) -> u64 {
    let mut graph = Graph::new();
    for (y, x) in (0..chars.len()).cartesian_product(0..chars[0].len()) {
        if chars[y][x] == '#' {
            continue;
        }

        let entry = graph.entry((y, x)).or_default();
        #[allow(clippy::match_on_vec_items)]
        let dirs = match chars[y][x] {
            '.' => vec![(-1, 0), (1, 0), (0, -1), (0, 1)],
            '<' => vec![(0, -1)],
            '>' => vec![(0, 1)],
            '^' => vec![(-1, 0)],
            'v' => vec![(1, 0)],
            _ => unreachable!(),
        };

        #[allow(
            clippy::cast_possible_wrap,
            clippy::cast_possible_truncation,
            clippy::cast_sign_loss
        )]
        for (dy, dx) in &dirs {
            if (y == 0 && *dy < 0) || (x == 0 && *dx < 0) {
                continue;
            }
            let ny = (y as isize + dy) as usize;
            let nx = (x as isize + dx) as usize;
            if chars
                .get(ny)
                .and_then(|line| line.get(nx))
                .is_some_and(|c| *c != '#')
            {
                entry.push((ny, nx, 1));
            }
        }
    }

    // Unlink corridor nodes (nodes with only two connections):
    let corridor_nodes = graph
        .iter()
        .filter(|(_, v)| v.len() == 2)
        .map(|(k, _)| *k)
        .collect_vec();
    for node in corridor_nodes {
        let neighbors = graph.remove(&node).unwrap();
        let (y1, x1, d1) = neighbors[0];
        let (y2, x2, d2) = neighbors[1];

        let n1 = graph.get_mut(&(y1, x1)).unwrap();
        if let Some(i) = n1.iter().position(|(y, x, _)| (*y, *x) == node) {
            n1[i] = (y2, x2, d1 + d2);
        }

        let n2 = graph.get_mut(&(y2, x2)).unwrap();
        if let Some(i) = n2.iter().position(|(y, x, _)| (*y, *x) == node) {
            n2[i] = (y1, x1, d1 + d2);
        }
    }

    dfs(
        &graph,
        &mut HashSet::new(),
        (chars.len() - 1, chars[0].len() - 2),
        (0, 1),
    )
    .unwrap()
}

fn dfs(
    graph: &Graph,
    seen: &mut HashSet<(usize, usize)>,
    goal: (usize, usize),
    current: (usize, usize),
) -> Option<u64> {
    if current == goal {
        return Some(0);
    }
    if seen.contains(&current) {
        return None;
    }
    seen.insert(current);

    let mut max = None;
    for (y, x, dist) in &graph[&current] {
        if let Some(d) = dfs(graph, seen, goal, (*y, *x)) {
            max = Some(max.unwrap_or(0).max(d + dist));
        }
    }

    seen.remove(&current);
    max
}
