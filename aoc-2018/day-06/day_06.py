import numpy as np
from itertools import count

points = np.loadtxt('input.txt', dtype=int, delimiter=', ')
# points = np.array([
#     [1, 1],
#     [1, 6],
#     [8, 3],
#     [3, 4],
#     [5, 5],
#     [8, 9]]
# )

def manhattan_spiral(x0=0, y0=0):
    """
    Generate lists of points where the Manhattan distance
    to (x0, y0) increases by one per list.

    For (x0, y0) = (0, 0):

    [(0, 0)]
    [(-1, 0), (0, 1), (0, -1), (1, 0)]
    [(-2, 0), (-1, 1), (-1, -1), (0, 2), (0, -2), (1, 1), (1, -1), (2, 0)]
    ...
    """
    for dist in count(0):
        points = []
        for i in range(-dist, dist + 1):
            points.append((x0 + i, y0 + dist - abs(i)))
            if dist != abs(i):
                points.append((x0 + i, y0 + abs(i) - dist))
        yield points

# Start at the center of the points (not so important where, but a reasonable start).
x0, y0 = [int(a) for a in np.mean(points, axis=0)]

# Need to at least include all the points in the spiral.
minimum_spiral_size = np.max(np.abs(x0 - points[:, 0]) + np.abs(y0 - points[:, 1]))

# Walk around the spiral. If one of the areas doesn't change (and we have gone
# far enough), then this point will not increase in the future either.
# Return the maximum area for such a finite point as soon as one is found.
areas_prev = np.zeros(len(points), dtype=int)
for i, coords in enumerate(manhattan_spiral(x0, y0)):
    areas = np.zeros_like(areas_prev)
    for x, y in coords:
        dists = np.abs(x - points[:, 0]) + np.abs(y - points[:, 1])
        closest_first = np.argmin(dists)
        closest_last = np.argmin(dists[::-1])

        if closest_first == len(points) - closest_last - 1:  # No tie, someone is closest
            areas[closest_first] += 1

    if i > minimum_spiral_size:
        bounded = areas < 1
        if np.any(areas_prev[bounded] > 0):
            print("Largest finite areas:", np.max((areas_prev + areas)[bounded]))
            break

    areas_prev += areas


# Part 2.
# Same idea, walk along the spiral and count the number of points that
# fulfill the criteria. If we have a non-empty region and it does not increase
# for one loop around the spiral, then we can stop because the region will
# not grow any more.
region_size = 0
for i, coords in enumerate(manhattan_spiral(x0, y0)):
    changed = False
    areas = np.zeros_like(areas_prev)
    for x, y in coords:
        dist = np.sum(np.abs(x - points[:, 0]) + np.abs(y - points[:, 1]))
        if dist < 10000:
            region_size += 1
            changed = True

    if region_size > 0 and not changed:
        print("Size of region with Manhattan distance to all points < 10000:", region_size)
        break
