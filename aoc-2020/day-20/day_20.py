import re

import numpy as np
from toolz import partition


def all_symmetries(tile, id_):
    rot = tile[:]
    rot_flip = tuple(np.flip(tile, axis=0))
    for i in range(4):
        yield f"{id_}_r{i}", rot
        yield f"{id_}_f{i}", rot_flip

        rot = tuple(map("".join, zip(*rot[::-1])))
        rot_flip = tuple(map("".join, zip(*rot_flip[::-1])))


def check_match_horizontal(t1, t2):
    return all(a[-1] == b[0] for a, b in zip(t1, t2))


def check_match_veritcal(t1, t2):
    return all(a == b for a, b in zip(t1[-1], t2[0]))


def solve(order):

    if len(order) == len(tiles):
        return order

    for id_ in {
        key
        for key in all_tiles
        if not any(key.startswith(o.split("_")[0]) for o in order)
    }:
        first = len(order) % n == 0 or check_match_horizontal(
            all_tiles[order[-1]], all_tiles[id_]
        )
        second = first and (
            len(order) < n or check_match_veritcal(all_tiles[order[-n]], all_tiles[id_])
        )
        if first and second and (res := solve(order + [id_])) is not None:
            return res


with open("input.txt") as f:
    tiles = {
        int(id_): tuple(s.strip().split("\n"))
        for id_, s in re.findall(r"Tile (\d+):\n((?:[.#]+\n?)+)", f.read())
    }

n = int(len(tiles) ** 0.5)
all_tiles = {
    f_id: sym for id_, tile in tiles.items() for f_id, sym in all_symmetries(tile, id_)
}


known_sol = solve([])
pure_ids = list(map(lambda s: int(s.split("_")[0]), known_sol))
print("Part 1:", pure_ids[0] * pure_ids[n - 1] * pure_ids[-n] * pure_ids[-1])

# Chop off borders.
image = [[s[1:-1] for s in all_tiles[id_][1:-1]] for id_ in known_sol]
image = "\n".join(("\n".join("".join(l) for l in zip(*s))) for s in partition(n, image))
image_split = image.split("\n")

sea_monster = """
                  # 
#    ##    ##    ###
 #  #  #  #  #  #   
""".strip(
    "\n"
).split(
    "\n"
)
sea_monster_width = len(sea_monster[0])

# For every symmetry of the image, for each possible offset in the image, see if the sea monster is there.
for _, image_sym in all_symmetries(image_split, ""):
    sea_monster_count = 0
    for i, line in enumerate(image_sym):
        if i + len(sea_monster) >= len(image_sym):
            break
        for j, c in enumerate(line):
            if j + sea_monster_width > len(line):
                break

            sea_monster_count += all(
                m != "#" or image_sym[i + di][j + dj] == "#"
                for di, monster_line in enumerate(sea_monster)
                for dj, m in enumerate(monster_line)
            )

    if sea_monster_count:
        print(
            "Part 2:",
            image.count("#") - sea_monster_count * "\n".join(sea_monster).count("#"),
        )
