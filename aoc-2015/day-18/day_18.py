import numpy as np
from numba import njit, prange

with open("input.txt") as f:
    lights = (
        np.asarray(list(map(list, f.read().strip().splitlines())), dtype=str) == "#"
    )


@njit(parallel=True)
def update(lights: np.ndarray, corners_fixed: bool):
    if corners_fixed:
        lights[0, 0] = lights[0, -1] = lights[-1, 0] = lights[-1, -1] = True
    new = lights.copy()

    for i in prange(lights.shape[0]):
        for j in prange(lights.shape[1]):
            neighbors_on = 0
            for di in [-1, 0, 1]:
                for dj in [-1, 0, 1]:
                    if (
                        (di or dj)
                        and (0 <= i + di < lights.shape[0])
                        and (0 <= j + dj < lights.shape[1])
                    ):
                        neighbors_on += lights[i + di, j + dj]
            new[i, j] = neighbors_on == 3 or (lights[i, j] and neighbors_on == 2)

    if corners_fixed:
        new[0, 0] = new[0, -1] = new[-1, 0] = new[-1, -1] = True
    return new


new = lights
for _ in range(100):
    new = update(new, corners_fixed=False)

print("Part 1:", np.sum(new))

new = lights
for _ in range(100):
    new = update(new, corners_fixed=True)

print("Part 2:", np.sum(new))
