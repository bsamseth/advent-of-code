#![allow(clippy::similar_names)]
use aocd::prelude::*;

use itertools::Itertools;

#[aocd(2023, 24)]
fn main() {
    let mut points = vec![];
    let splitter = |s: &str| {
        s.split_terminator(", ")
            .map(|s| s.trim_start().parse::<i128>().unwrap())
            .collect_tuple()
            .unwrap()
    };
    for line in input!().lines() {
        let (pos, vel) = line.split_once(" @ ").unwrap();
        points.push(Hailstone {
            pos: splitter(pos),
            vel: splitter(vel),
        });
    }

    let mut intersections = 0;
    for (i, a) in points.iter().enumerate() {
        for b in &points[i + 1..] {
            if a == b {
                continue;
            }

            // Compute the intersection point of the lines, if any. The intersection point must be
            // in the future, so we disregard any pair of lines that intersect in the past.
            let (x1, y1, _) = a.pos;
            let (x2, y2, _) = b.pos;
            let (vx1, vy1, _) = a.vel;
            let (vx2, vy2, _) = b.vel;

            let determinant = vx1 * vy2 - vx2 * vy1;
            if determinant == 0 {
                // Parallel lines don't intersect.
                continue;
            }

            let t = (vy2 * (x2 - x1) - vx2 * (y2 - y1)) / determinant;
            let s = (vy1 * (x2 - x1) - vx1 * (y2 - y1)) / determinant;

            if t < 0 || s < 0 {
                continue;
            }

            let x_int = x1 + vx1 * t;
            let y_int = y1 + vy1 * t;

            if (200_000_000_000_000..=400_000_000_000_000).contains(&x_int)
                && (200_000_000_000_000..=400_000_000_000_000).contains(&y_int)
            {
                intersections += 1;
            }
        }
    }
    submit!(1, intersections);

    // Direct solution based on the following insight:
    // Transform coordinates relative to one of the hailstones, say the zeroth. The collision of
    // the rock and this hailstone will then by definition be at the origin. The lines from the
    // origin to all the other collision points must be collinear if the rock is to move in a
    // straight line. The collision points for two other hailstones are given by:
    //
    //   Hailstone 1: p1 + t1 * v1,
    //   Hailstone 2: p2 + t2 * v2,
    //
    // where p1, p2, v1, v2, are the positions and velocities of the hailstones, respectively.
    // Note that these coordinates are in the zeroth hailstone's frame of reference.
    //
    // The collinearity condition is given by their dot product being zero:
    //
    //   (p1 + t1 v1) * (p2 + t2 v2) = 0
    //
    // Some algebra and orthogonality arguments later we can get these two equations:
    //
    //   t1 = - (p1 x p2) * v2 / (v1 x p2) * v2
    //   t2 = - (p1 x p2) * v1 / (p1 x v2) * v1
    //
    // These we can compute directly and use to find the position of the rock at t = 0:
    //
    //   v_rock = (c2 - c1) / (t2 - t1)
    //   p_rock = c1 - t1 * v_rock
    //
    // Finally, we shift back to the original coordinates by adding the zeroth hailstone's position.
    let p0 = points[0].pos;
    let v0 = points[0].vel;
    let p1 = points[1].pos;
    let v1 = points[1].vel;
    let p2 = points[2].pos;
    let v2 = points[2].vel;

    let p1 = sub(p1, p0);
    let p2 = sub(p2, p0);
    let v1 = sub(v1, v0);
    let v2 = sub(v2, v0);

    let t1 = -dot(cross(p1, p2), v2) / (dot(cross(v1, p2), v2));
    let t2 = -dot(cross(p1, p2), v1) / dot(cross(p1, v2), v1);

    let c1 = add(p1, mul(v1, t1));
    let c2 = add(p2, mul(v2, t2));

    let v_rock = div(sub(c2, c1), t2 - t1);
    let p_rock = sub(c1, mul(v_rock, t1));

    let p_rock = add(p_rock, p0);
    submit!(2, p_rock.0 + p_rock.1 + p_rock.2);
}

#[derive(Debug, Clone, PartialEq)]
struct Hailstone {
    pos: V3<i128>, // Using i128 to avoid overflows during calculations.
    vel: V3<i128>,
}

type V3<T> = (T, T, T);

fn add<T: std::ops::Add<Output = T>>(a: V3<T>, b: V3<T>) -> V3<T> {
    (a.0 + b.0, a.1 + b.1, a.2 + b.2)
}

fn sub<T: std::ops::Sub<Output = T>>(a: V3<T>, b: V3<T>) -> V3<T> {
    (a.0 - b.0, a.1 - b.1, a.2 - b.2)
}

fn mul<T: std::ops::Mul<Output = T> + Copy>(a: V3<T>, b: T) -> V3<T> {
    (a.0 * b, a.1 * b, a.2 * b)
}

fn div<T: std::ops::Div<Output = T> + Copy>(a: V3<T>, b: T) -> V3<T> {
    (a.0 / b, a.1 / b, a.2 / b)
}

fn dot<T: std::ops::Add<Output = T> + std::ops::Mul<Output = T>>(a: V3<T>, b: V3<T>) -> T {
    a.0 * b.0 + a.1 * b.1 + a.2 * b.2
}

fn cross<T: std::ops::Sub<Output = T> + std::ops::Mul<Output = T> + Copy>(
    a: V3<T>,
    b: V3<T>,
) -> V3<T> {
    (
        a.1 * b.2 - a.2 * b.1,
        a.2 * b.0 - a.0 * b.2,
        a.0 * b.1 - a.1 * b.0,
    )
}
