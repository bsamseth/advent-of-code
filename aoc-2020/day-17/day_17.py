import copy
from collections import namedtuple

Point = namedtuple("Point", ["z", "y", "x"])


def bounds(space):
    z, y, x = zip(*(point for point, state in space.items() if state == "#"))
    return Point(min(z), min(y), min(x)), Point(max(z), max(y), max(x))


def show(space):
    min_bound, max_bound = bounds(space)
    for z in range(min_bound.z, max_bound.z + 1):
        print(f"\n{z=}")
        for y in range(-1 + min_bound.y, max_bound.y + 1):
            for x in range(-1 + min_bound.x, max_bound.x + 1):
                print(space.get((z, y, x), "."), end="")
            print()


with open("input.txt") as f:
    init = [list(row) for row in f.read().splitlines()]

space = {
    Point(z=0, y=j, x=i): state
    for j, row in enumerate(init)
    for i, state in enumerate(row)
}

for _ in range(6):
    new_space = copy.deepcopy(space)
    min_bound, max_bound = bounds(space)
    for z in range(-1 + min_bound.z, max_bound.z + 2):
        for y in range(-1 + min_bound.y, max_bound.y + 2):
            for x in range(-1 + min_bound.x, max_bound.x + 2):
                active_neighbors = 0
                for dz in (-1, 0, 1):
                    for dy in (-1, 0, 1):
                        for dx in (-1, 0, 1):
                            if dz or dy or dx:
                                active_neighbors += (
                                    space.get((z + dz, y + dy, x + dx)) == "#"
                                )
                if (space.get((z, y, x)) == "#" and 2 <= active_neighbors <= 3) or (
                    space.get((z, y, x)) != "#" and active_neighbors == 3
                ):
                    new_space[(z, y, x)] = "#"
                elif (z, y, x) in new_space:
                    del new_space[(z, y, x)]
    space = new_space

print("Part 1:", len(space))
