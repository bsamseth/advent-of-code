import numpy as np
from parse import parse


with open("input.txt") as f:
    ops = [parse("{op} {x0:d},{y0:d} through {x1:d},{y1:d}", line) for line in f]

lights = np.zeros((1000, 1000), dtype=bool)
for op in ops:
    region = slice(op["x0"], op["x1"] + 1), slice(op["y0"], op["y1"] + 1)

    if op["op"] == "toggle":
        lights[region] = ~lights[region]
    else:
        lights[region] = op["op"] == "turn on"

print("Part 1:", np.sum(lights))

lights = np.zeros((1000, 1000), dtype=int)
for op in ops:
    region = slice(op["x0"], op["x1"] + 1), slice(op["y0"], op["y1"] + 1)

    if op["op"] == "toggle":
        lights[region] += 2
    elif op["op"] == "turn on":
        lights[region] += 1
    else:
        lights[region] = np.maximum(lights[region] - 1, 0)

print("Part 2:", np.sum(lights))
