use std::cell::RefCell;
use std::fmt::Display;

use aocd::*;

#[derive(Debug, Clone)]
struct Node {
    x: i64,
    next: usize,
    prev: usize,
    me: usize,
}

#[derive(Debug)]
struct List {
    zero: usize,
    nodes: Vec<RefCell<Node>>,
}

impl List {
    // Move the node at index i forward by the value of its is field x.
    fn shift(&mut self, i: usize) {
        #[allow(unused_assignments)]
        let mut target_index = 0;
        {
            let mut target = self.nodes[i].borrow_mut();
            for _ in 0..=(target.x).rem_euclid(self.nodes.len() as i64 - 1) as usize {
                target = self.nodes[target.next].borrow_mut();
            }
            target_index = target.me;
        }
        if target_index == i {
            return;
        }

        // Unlink the node at index i.
        {
            let node = self.nodes[i].borrow();
            let mut prev = self.nodes[node.prev].borrow_mut();
            let mut next = self.nodes[node.next].borrow_mut();
            prev.next = node.next;
            next.prev = node.prev;
        }

        // Link it back in after the target.
        let mut node = self.nodes[i].borrow_mut();
        let mut target = self.nodes[target_index].borrow_mut();
        let mut target_prev = self.nodes[target.prev].borrow_mut();
        node.next = target_index;
        node.prev = target.prev;
        target_prev.next = i;
        target.prev = i;
    }

    fn at(&self, i: usize) -> i64 {
        let mut node = self.nodes[self.zero].borrow();
        for _ in 0..(i.rem_euclid(self.nodes.len())) {
            node = self.nodes[node.next].borrow();
        }
        node.x
    }
}

impl Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut as_vec = Vec::with_capacity(self.nodes.len());
        let mut node = self.nodes[self.zero].borrow();
        loop {
            as_vec.push(node.x);
            node = self.nodes[node.next].borrow();
            if node.me == self.zero {
                break;
            }
        }
        write!(f, "{:?}", as_vec)
    }
}

impl From<Vec<i64>> for List {
    fn from(v: Vec<i64>) -> Self {
        let mut nodes = Vec::with_capacity(v.len());
        let mut zero = 0;
        for (i, x) in v.iter().enumerate() {
            if *x == 0 {
                zero = i;
            }
            nodes.push(RefCell::new(Node {
                x: *x,
                next: (i + 1) % v.len(),
                prev: (i + v.len() - 1) % v.len(),
                me: i,
            }));
        }
        List { zero, nodes }
    }
}

#[aocd(2022, 20)]
fn main() {
    let numbers = input!()
        .lines()
        .map(|line| line.parse().unwrap())
        .collect::<Vec<i64>>();
    let numbers_decrypted = numbers.iter().map(|x| x * 811589153).collect::<Vec<_>>();

    let mut list: List = numbers.clone().into();
    let mut list_decrypted: List = numbers_decrypted.clone().into();

    for i in 0..numbers.len() {
        list.shift(i);
    }
    for _ in 0..10 {
        for i in 0..numbers.len() {
            list_decrypted.shift(i);
        }
    }

    let part1: i64 = list.at(1000) + list.at(2000) + list.at(3000);
    let part2: i64 = list_decrypted.at(1000) + list_decrypted.at(2000) + list_decrypted.at(3000);
    submit!(1, part1);
    submit!(2, part2);
}
