import numpy as np
import re
from itertools import combinations
from math import gcd


def lcm(a, b, *args):
    if not args:
        return a * b // gcd(a, b)
    return lcm(lcm(a, b), *args)


def energy(x, v):
    return np.sum(np.sum(np.abs(x), axis=1) * np.sum(np.abs(v), axis=1))


def iterate(x, v, energy_at=1000):
    N, D = x.shape
    x0, v0 = x.copy(), v.copy()
    periods = np.array(
        [0, 0, 0]
    )  # Per dim. Set to negative numbers once a period is found.
    iteration = 0
    while any(p >= 0 for p in periods):

        # Look for a repeat:
        for k, period in enumerate(periods):
            if (
                period > 0
                and np.all(x[:, k] == x0[:, k])
                and np.all(v[:, k] == v0[:, k])
            ):
                periods[k] *= -1
            elif period >= 0:
                periods[k] += 1

        # Update velocity and position:
        for i, j in combinations(range(N), 2):
            diff = np.sign(x[j, :] - x[i, :])
            v[i, :] += diff
            v[j, :] -= diff
        x += v

        iteration += 1
        if iteration == energy_at:
            print("Part 1:", energy(x, v))
    return -periods


with open("input.txt") as f:
    x = np.array([int(n.group()) for n in re.finditer(r"-?\d+", f.read())])

x = x.reshape(-1, 3)
v = np.zeros_like(x)
print(f"Part 2: {lcm(*iterate(x, v, energy_at=1000))}")
