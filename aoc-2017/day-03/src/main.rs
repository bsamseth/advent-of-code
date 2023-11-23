use aocd::prelude::*;
use num::complex::Complex;

#[aocd(2017, 3)]
fn main() {
    let target_square = input!().parse::<u64>().unwrap();

    let mut memory: std::collections::HashMap<Complex<i64>, u64> = std::collections::HashMap::new();
    let rot_90 = Complex::new(0, 1);
    let mut square = 1;
    let mut coord = Complex::new(0, 0);
    let mut dir = Complex::new(1, 0);
    let mut steps = 1;
    memory.insert(coord, square);

    let mut do_part2 = true;
    loop {
        for _ in 0..2 {
            for _ in 0..steps {
                square += 1;
                coord += dir;

                if square == target_square {
                    submit!(1, coord.re.abs() + coord.im.abs());
                    return;
                }

                if do_part2 {
                    let value = [
                        Complex::new(1, 0),
                        Complex::new(1, 1),
                        Complex::new(0, 1),
                        Complex::new(-1, 1),
                        Complex::new(-1, 0),
                        Complex::new(-1, -1),
                        Complex::new(0, -1),
                        Complex::new(1, -1),
                    ]
                    .iter()
                    .map(|&offset| coord + offset)
                    .map(|coord| memory.get(&coord).unwrap_or(&0))
                    .sum();
                    memory.insert(coord, value);

                    if value > target_square {
                        do_part2 = false;
                        submit!(2, value);
                    }
                }
            }
            dir *= rot_90;
        }
        steps += 1;
    }
}
