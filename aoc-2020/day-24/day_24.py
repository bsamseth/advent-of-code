import re
import time
from numpy import array
from collections import Counter
from tqdm import trange

t0 = time.time()

directions = {
    "e": array([1, -1, 0]),
    "w": array([-1, 1, 0]),
    "ne": array([1, 0, -1]),
    "nw": array([0, 1, -1]),
    "se": array([0, -1, 1]),
    "sw": array([-1, 0, 1]),
}

with open("input.txt") as f:
    lines = [re.findall(r"e|nw|ne|w|sw|se", line) for line in f]


tile_counts = Counter(tuple(sum(directions[step] for step in line)) for line in lines)
tiles = {tile: count % 2 == 1 for tile, count in tile_counts.items()}
print("Part 1:", sum(tiles.values()))


for _ in trange(100):
    new_tiles = tiles.copy()
    for tile, state in tiles.items():
        for offset in [array([0, 0, 0])] + list(directions.values()):
            t = tuple(tile + offset)
            if t in tiles and offset.any():
                continue

            black_neighbors = sum(
                tiles.get(tuple(t + d), 0) for d in directions.values()
            )
            state = tiles.get(t, False)
            if (state and (black_neighbors == 0 or black_neighbors > 2)) or (
                not state and black_neighbors == 2
            ):
                new_tiles[t] = not state
    tiles = new_tiles

print("Part 2:", sum(tiles.values()))
print(f"Total time: {round(time.time()-t0, 2)} seconds")
