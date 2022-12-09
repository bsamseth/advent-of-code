import operator

from aocd import data, submit


def step(head: complex, tail: complex) -> tuple[complex, complex]:
    diff = head - tail
    length = diff.real**2 + diff.imag**2
    comp = operator.ge if length > 4 else operator.gt
    x, y = [u // abs(u) if comp(abs(u),1) else 0 for u in (diff.real, diff.imag)]
    return head, tail + x + y * 1j

knots = [0j] * 10
coords_1, coords_9 = set(), set()
for line in data.splitlines():
    dir, count = {'L': -1, 'R': 1, 'U': 1j, 'D': -1j}[line[0]], int(line[2:])
    for _ in range(count):
        knots[0] += dir
        for i in range(len(knots)-1):
            knots[i:i+2] = step(*knots[i:i+2])
        coords_1.add(knots[1])
        coords_9.add(knots[-1])
            
submit(part=1, answer=len(coords_1))
submit(part=2, answer=len(coords_9))
