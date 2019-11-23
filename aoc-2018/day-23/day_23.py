import re
from dataclasses import dataclass

import numpy as np


@dataclass
class Nano:
    x: int
    y: int
    z: int
    r: int

    @property
    def pos(self):
        return np.array([self.x, self.y, self.z])

    def __sub__(self, other):
        return np.sum(np.abs(self.pos - other.pos))


with open("input.txt") as f:
    data = f.read().strip()

nanos = [
    Nano(*[int(d) for d in nanobot.groups()])
    for nanobot in re.finditer(r"pos=<(-?\d+),(-?\d+),(-?\d+)>,\sr=(-?\d+)", data)
]

strongest = max(nanos, key=lambda nano: nano.r)

print(f"Part 1: {sum(strongest - n <= strongest.r for n in nanos)}")
