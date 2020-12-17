import copy
from collections import namedtuple

Point = namedtuple("Point", ["z", "y", "x", "w"])


def bounds(space):
    z, y, x, w = zip(*(point for point, state in space.items() if state == "#"))
    return Point(min(z), min(y), min(x), min(w)), Point(max(z), max(y), max(x), max(w))


with open("input.txt") as f:
    init = [list(row) for row in f.read().splitlines()]

space = {
    Point(z=0, y=j, x=i, w=0): state
    for j, row in enumerate(init)
    for i, state in enumerate(row)
}

for _ in range(6):
    new_space = copy.deepcopy(space)
    min_bound, max_bound = bounds(space)
    for z in range(-1 + min_bound.z, max_bound.z + 2):
        for y in range(-1 + min_bound.y, max_bound.y + 2):
            for x in range(-1 + min_bound.x, max_bound.x + 2):
                for w in range(-1 + min_bound.w, max_bound.w + 2):
                    active_neighbors = 0
                    for dz in (-1, 0, 1):
                        for dy in (-1, 0, 1):
                            for dx in (-1, 0, 1):
                                for dw in (-1, 0, 1):
                                    if dz or dy or dx or dw:
                                        active_neighbors += (
                                            space.get((z + dz, y + dy, x + dx, w + dw))
                                            == "#"
                                        )
                    if (
                        space.get((z, y, x, w)) == "#" and 2 <= active_neighbors <= 3
                    ) or (space.get((z, y, x, w)) != "#" and active_neighbors == 3):
                        new_space[(z, y, x, w)] = "#"
                    elif (z, y, x, w) in new_space:
                        del new_space[(z, y, x, w)]
    space = new_space

print("Part 2:", len(space))
