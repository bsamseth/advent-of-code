import io
from heapq import heappop, heappush

import numpy as np
from aocd import data, submit

# Initial solution (which worked fine) cleaned up based on inspiration from subreddit user ViliamPucik
# Small optimization and refactoring compared with the inspiration post.
grid = np.genfromtxt(io.StringIO(data), delimiter=1, dtype=int)
height, width = grid.shape

for map_scale in 1, 5:
    heap, seen = [(0, 0, 0)], {(0, 0)}

    while heap:
        risk, r, c = heappop(heap)
        if r == map_scale * height - 1 and c == map_scale * width - 1:
            submit(risk, part=(map_scale == 5) + 1)
            break

        for r_, c_ in (r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1):
            if (
                (r_, c_) not in seen
                and 0 <= r_ < map_scale * height
                and 0 <= c_ < map_scale * width
            ):
                rd, rm = divmod(r_, height)
                cd, cm = divmod(c_, width)

                seen.add((r_, c_))
                heappush(heap, (risk + (grid[rm][cm] + rd + cd - 1) % 9 + 1, r_, c_))
