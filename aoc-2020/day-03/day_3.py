from functools import reduce
from operator import mul

with open("input.txt") as f:
    trees = [line.strip() for line in f]


def tree_count(slope):
    dy, dx = slope
    return sum(
        trees[dy * i][(dx * i) % len(trees[0])] == "#"
        for i in range(0, len(trees) // dy)
    )


slopes = list(map(tree_count, [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]))

print(f"Part 1: {slopes[1]}")
print(f"Part 2: {reduce(mul, slopes)}")
