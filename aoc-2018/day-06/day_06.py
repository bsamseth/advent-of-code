import numpy as np

points = np.loadtxt('input.txt', dtype=int, delimiter=', ')

# Part 1:
# Areas that do not change if we increase the grid boundaries
# are the areas that are finite. Expand the grid until such
# bounded areas are found, then pick the maximum.
def areas(x_min, x_max, y_min, y_max, points):
    areas = np.zeros(len(points), dtype=np.uint32)
    X = np.arange(x_min, x_max)
    Y = np.arange(y_min, y_max)
    X, Y = np.meshgrid(X, Y)
    for x, y in zip(X.ravel(), Y.ravel()):
        # Calculate all distances.
        dists = np.abs(x - points[:, 0]) + np.abs(y - points[:, 1])
        closest_first = np.argmin(dists)
        closest_last = np.argmin(dists[::-1])

        if closest_first == len(points) - closest_last - 1:  # No tie, someone is closest
            areas[closest_first] += 1

    return areas


x_min, y_min = np.min(points, axis=0) - 5
x_max, y_max = np.max(points, axis=0) + 5

areas_prev = areas(x_min, x_max, y_min, y_max, points)
while True:
    x_min -= 5
    x_max += 5
    y_min -= 5
    y_max += 5

    areas_new = areas(x_min, x_max, y_min, y_max, points)

    bounded = areas_new - areas_prev == 0
    if np.any(bounded):
        print(np.max(areas_new[bounded]))
        break


