import re
from dataclasses import dataclass

from z3 import Int, Optimize, If


@dataclass
class Nano:
    x: int
    y: int
    z: int
    r: int

    @property
    def pos(self):
        return self.x, self.y, self.z

    def __sub__(self, other):
        return sum(abs(a - b) for a, b in zip(self.pos, other.pos))


def zabs(x):
    return If(x >= 0, x, -x)


with open("input.txt") as f:
    nanos = [Nano(*[int(m.group()) for m in re.finditer(r"-?\d+", line)]) for line in f]

strongest = max(nanos, key=lambda nano: nano.r)
print(f"Part 1: {sum(strongest - n <= strongest.r for n in nanos)}")


# Variables for the target coordinate.
x, y, z = Int("x"), Int("y"), Int("z")
# Boolean per nano, 1 if (x, y, z) is in range.
in_ranges = [Int("in_range_" + str(i)) for i, _ in enumerate(nanos)]
# The number of nanos in range.
range_count = Int("sum")

o = Optimize()
for nano, in_range in zip(nanos, in_ranges):
    (nx, ny, nz), r = nano.pos, nano.r
    o.add(in_range == If(zabs(x - nx) + zabs(y - ny) + zabs(z - nz) <= r, 1, 0))

# Maximize the number of nanos in range.
o.add(range_count == sum(in_ranges))
h1 = o.maximize(range_count)

# Break ties by minimizing the distance from the origin.
dist_from_zero = Int("dist")
o.add(dist_from_zero == zabs(x) + zabs(y) + zabs(z))
h2 = o.minimize(dist_from_zero)

print(o.check())  # Check feasibility.
lower, upper = o.lower(h2), o.upper(h2)  # Bounds on dist_from_zero.
assert lower == upper, "Didn't find unique solution..."
print(f"Part 2: {lower}")
