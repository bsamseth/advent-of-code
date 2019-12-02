import re
from functools import lru_cache
from heapq import heappush, heappop

ROCKY, WET, NARROW = 0, 1, 2
NEITHER, TORCH, CLIMB = 0, 1, 2
depth, tx, ty = [int(d) for d in re.findall(r"-?\d+", open("input.txt").read())]
target = tx + 1j * ty


@lru_cache(maxsize=None)
def erosion(pos):
    x, y = int(pos.real), int(pos.imag)
    if y == ty and x == tx:
        geoindex = 0
    elif y == 0:
        geoindex = x * 16807
    elif x == 0:
        geoindex = y * 48271
    else:
        geoindex = erosion(pos - 1j) * erosion(pos - 1)

    return (geoindex + depth) % 20183


def typeof(pos):
    return erosion(pos) % 3


print("Part 1:", sum(typeof(x + 1j * y) for x in range(tx + 1) for y in range(ty + 1)))

queue = [(0, 0, 0, TORCH)]  # (time, x, y, equipment)
visited = {(0, TORCH): 0}  # (pos, equipment): time

while True:
    time, x, y, eq = heappop(queue)
    pos = x + 1j * y
    if (pos, eq) == (target, TORCH):
        print(f"Part 2: {time}")
        break

    for t, e in [(time + 1, eq), (time + 8, (typeof(pos) | eq) ^ 0b11)]:
        for n in [pos + 1, pos - 1, pos + 1j, pos - 1j]:
            if (
                n.real < 0
                or n.imag < 0
                or typeof(n) == e
                or ((n, e) in visited and visited[(n, e)] <= t)
            ):
                continue
            visited[(n, e)] = t
            heappush(queue, (t, n.real, n.imag, e))
