from itertools import count, product

import numpy as np

grid = np.genfromtxt("input.txt", delimiter=1, dtype=int)


def propagate_flash(grid, i, j, flash_mask):
    flash_mask[i, j] = 1
    for di, dj in product(range(-1, 2), range(-1, 2)):
        if 0 <= i + di < grid.shape[0] and 0 <= j + dj < grid.shape[1]:
            if flash_mask[i + di, j + dj] == 0:
                grid[i + di, j + dj] += 1
                if grid[i + di, j + dj] > 9:
                    propagate_flash(grid, i + di, j + dj, flash_mask)


flash_count = 0
for step in count(1):
    flash_mask = grid != grid
    grid += 1
    for i, j in np.ndindex(*grid.shape):
        if not flash_mask[i, j] and grid[i, j] > 9:
            propagate_flash(grid, i, j, flash_mask)
    grid[flash_mask] = 0
    if step <= 100:
        flash_count += np.sum(flash_mask)
    elif flash_mask.all():
        break


print("Part 1:", flash_count)
print("Part 2:", step)
