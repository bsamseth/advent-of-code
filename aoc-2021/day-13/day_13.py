import re

import numpy as np
import pytesseract
from aocd import data, submit
from PIL import Image

points = np.array([(int(y), int(x)) for x, y in re.findall(r"(\d+),(\d+)", data)])
folds = [(x, int(y)) for x, y in re.findall(r"(x|y)=(\d+)", data)]

maxy, maxx = points.max(axis=0)
grid = np.zeros((maxy + 1, maxx + 1), dtype=int)
grid[points[:, 0], points[:, 1]] = 1

for i, (direction, z) in enumerate(folds):
    if direction == "y":
        left = grid[:z]
        right = grid[-1:z:-1]
        right = np.pad(right, ((left.shape[0] - right.shape[0], 0), (0, 0)))
        grid = left | right
    else:
        up = grid[:, :z]
        down = grid[:, -1:z:-1]
        down = np.pad(down, ((0, 0), (up.shape[1] - down.shape[1], 0)))
        grid = up | down
    if i == 0:
        submit(np.sum(grid), part=1)

"""
To just see the code:
import matplotlib.pyplot as plt
plt.imshow(grid, cmap="binary")
plt.show()
"""
answer = (
    pytesseract.image_to_string(
        Image.open("/home/bendik/Downloads/day_13.png"), config="--psm 6"
    )
    .replace(" ", "")
    .splitlines()[0]
)
submit(answer, part=2)
