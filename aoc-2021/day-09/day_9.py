from collections import defaultdict

import numpy as np

grid = np.pad(
    np.genfromtxt("input.txt", delimiter=1, dtype=int),
    1,
    "constant",
    constant_values=10,
)
risk_sum = 0
for i, j in np.ndindex(*grid.shape):
    if all(
        grid[i + di, j + dj] > grid[i, j]
        for di, dj in ((-1, 0), (1, 0), (0, -1), (0, 1))
    ):
        risk_sum += grid[i, j] + 1

print("Part 1:", risk_sum)


def expand_basin(id_, i, j):
    if (i, j) in seen:
        return
    seen.add((i, j))
    if grid[i, j] >= 9:
        return

    basins[id_].add((i, j))
    for di, dj in ((-1, 0), (1, 0), (0, -1), (0, 1)):
        expand_basin(id_, i + di, j + dj)


seen = set()
basins = defaultdict(set)
basin_id = 0
for i, j in np.ndindex(*grid.shape):
    if grid[i, j] < 9 and (i, j) not in seen:
        expand_basin(basin_id, i, j)
        basin_id += 1

a, b, c, *_ = sorted(map(len, basins.values()), reverse=True)
print("Part 2:", a * b * c)
