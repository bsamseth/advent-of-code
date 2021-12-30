import re

import numpy as np
from aocd import data, submit

x0, x1, y0, y1 = map(int, re.findall(r"-?\d+", data))


def simulate(u, v):
    x, y = 0, 0
    while y > y0 and x <= x1:
        x += u
        y += v
        u -= np.sign(u)
        v -= 1

        if x < x0 and u < 1:
            break
        if x0 <= x <= x1 and y0 <= y <= y1:
            return True
    return False


min_u = int(np.ceil(0.5 * np.sqrt(8 * x0 + 1) - 0.5))
max_u = x1
min_v = y0
max_v = abs(y0) - 1

submit(max_v * (max_v + 1) // 2, part=1)
submit(
    sum(
        simulate(u, v) for u in range(min_u, max_u + 1) for v in range(min_v, max_v + 1)
    ),
    part=2,
)
