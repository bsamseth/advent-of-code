from collections import defaultdict

import numpy as np

with open("input.txt") as f:
    lines = np.array(
        [
            [int(x) for x in line.replace(" -> ", ",").split(",")]
            for line in f.read().split("\n")
        ]
    )

part1_grid = defaultdict(int)
part2_grid = defaultdict(int)
for point in lines:
    c0, c1 = point.reshape(2, 2)
    u = np.sign(c1 - c0)
    x = c0
    while True:
        part1_grid[tuple(x)] += not u.all()
        part2_grid[tuple(x)] += 1
        if (x == c1).all():
            break
        x += u

print("Part 1:", sum(1 for v in part1_grid.values() if v > 1))
print("Part 2:", sum(1 for v in part2_grid.values() if v > 1))
