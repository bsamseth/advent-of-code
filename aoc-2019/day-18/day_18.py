import numpy as np
import time

with open("input-test.txt") as f:
    grid = [list(line.strip()) for line in f]


def print_grid(grid, rewrite=False):
    # return
    time.sleep(0.01)
    height, width = np.shape(grid)

    if rewrite:
        print("\033[{}A".format(height), end="")

    for row in grid:
        print("".join(row))


def walk(grid, i, j, last, required_keys, step_count=0):
    c = grid[i][j]
    if c == "#" or c.isupper():
        return False, step_count

    grid[i][j] = "@"
    print_grid(grid, rewrite=True)
    grid[i][j] = c

    if c.islower() and c in required_keys:
        required_keys = [mark for mark in required_keys if mark != c]
        grid = [[d if d != c.upper() else "." for d in row] for row in grid]

        last = None
        if not required_keys:
            return True, step_count

    if (
        sum(
            grid[i + di][j + dj] == "#" for di, dj in ((0, -1), (0, 1), (-1, 0), (1, 0))
        )
        == 3
    ):
        grid = [
            ["#" if (k == i and l == j) else d for l, d in enumerate(row)]
            for k, row in enumerate(grid)
        ]

    best_count = float("inf")
    for di, dj in ((0, -1), (0, 1), (-1, 0), (1, 0)):
        if (i + di, j + dj) == last:
            continue

        done, count = walk(grid, i + di, j + dj, (i, j), required_keys, step_count + 1,)

        if done:
            best_count = min(best_count, count)

    if best_count < float("inf"):
        return True, best_count

    return False, best_count


required_keys = [c for row in grid for c in row if c.islower()]
start_i, start_j = [int(d) for d in np.where(np.asarray(grid) == "@")]
grid[start_i][start_j] = "."
# print(grid)
print_grid(grid)
print("Part 1:", walk(grid, start_i, start_j, None, required_keys)[1])
