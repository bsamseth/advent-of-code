import numpy as np
from math import gcd, atan2
from itertools import product


def directions(root_x, root_y):
    """Return all valid directions, starting north and rotating clockwise.

    Directions are given as (x, y) vectors. These are sorted by increasing angle to the (0, -1) line.
    All directions valid on a grid of size (2 * root_x, 2 * root_y) are generated.

    >>> np.array(directions(2, 2))
    array([[ 0, -1],     # 1
           [ 1, -2],     # 2
           [ 1, -1],     # 3
           [ 2, -1],     # 4
           [ 1,  0],     # 5
           [ 2,  1],     # 6
           [ 1,  1],     # 7
           [ 1,  2],     # 8
           [ 0,  1],     # 9
           [-1,  2],     # A
           [-1,  1],     # B
           [-2,  1],     # C
           [-1,  0],     # D
           [-2, -1],     # E
           [-1, -1],     # F
           [-1, -2]])    # G

    Because, for root at X = (2, 2), the directions have this ordering:

        .G.2..
        EF134.
        .DX5..
        CB976.
        .A.8..
        ......
    """
    dirs = set()
    for x, y in product(*map(range, (root_x + 1, root_y + 1))):

        dx, dy = x - root_x, y - root_y
        if not dx and not dy:
            continue
        elif not dx:
            dirs.add(1j if dy >= 0 else -1j)
        elif not dy:
            dirs.add(1 if dx >= 0 else -1)
        else:
            div = gcd(dx, dy)
            dx, dy = dx // div, dy // div
            dirs.add(dx + 1j * dy)

    ordered = sorted(dirs, key=lambda z: atan2(z.imag, z.real))
    final = []
    for z in (1j, -1, -1j, 1):
        for x in ordered[:-1]:
            x *= z
            final.append((int(x.real), int(x.imag)))

    # Shift right once so that (0, -1) is the first element.
    return [final[-1]] + final[:-1]


def line_of_sight_count(grid, x, y, dirs):
    count = 0

    for dx, dy in dirs:
        ddx, ddy = dx, dy

        while 0 <= x + ddx < grid.shape[1] and 0 <= y + ddy < grid.shape[0]:
            if grid[y + ddy, x + ddx]:
                count += 1
                break
            ddx += dx
            ddy += dy

    return count


def laser(grid, x, y, dirs, target=200):
    grid = grid[:, :]
    count = 0

    while np.sum(grid):
        for dx, dy in dirs:
            ddx, ddy = dx, dy

            while 0 <= x + ddx < grid.shape[1] and 0 <= y + ddy < grid.shape[0]:
                if grid[y + ddy, x + ddx]:
                    count += 1
                    grid[y + ddy, x + ddx] = False

                    if count == target:
                        return (x + ddx) * 100 + y + dy

                    break
                ddx += dx
                ddy += dy

        return count


with open("input.txt") as f:
    grid = np.array([[c == "#" for c in line.strip()] for line in f])

# Pre-generate a list of directions. If this doesn't work for your input, increase n.
n = sum(grid.shape) // 2
dirs = directions(n, n)

biggest_count, point = max(
    (line_of_sight_count(grid, x, y, dirs) if grid[y, x] else 0, (x, y))
    for x, y in product(*list(map(range, grid.shape)))
)

print("Part 1:", biggest_count)
print("Part 2:", laser(grid, point[0], point[1], dirs, target=200))
