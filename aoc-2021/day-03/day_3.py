import numpy as np
from toolz import compose, first, pipe
from toolz.curried import map

with open("input.txt", "r") as f:
    x = np.array([[int(b) for b in line.strip()] for line in f])

most_common = x.sum(axis=0) >= x.shape[0] / 2
gamma, epsilon = pipe(
    most_common,
    map(bool),
    map(lambda maximize: sorted(("0", "1"), reverse=maximize)),
    lambda x: zip(*x),
    map("".join),
    map(lambda s: int(s, 2)),
)

print("Part 1:", gamma * epsilon)


def rating(b):
    candidates = set(map(compose("".join, map(str)), x.tolist()))
    for i in range(x.shape[1]):
        most_common = (
            sum(candidate[i] == "1" for candidate in candidates) >= len(candidates) / 2
        )
        candidates = {
            candidate for candidate in candidates if (candidate[i] == b) == most_common
        }
        if len(candidates) < 2:
            return int(first(candidates), 2)


print("Part 2:", rating("1") * rating("0"))
