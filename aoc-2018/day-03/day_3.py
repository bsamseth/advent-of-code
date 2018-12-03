import re
import numpy as np

with open('input1.txt', 'r') as f:
    data = f.read()


# Part 1
# For each square, keep track of how many claims have been made
# for it. The size is limited at 1000 x 1000 in the text.
fabric = np.zeros((1000, 1000), dtype=int)

for match in re.finditer("#([0-9]+)\s@\s([0-9]+),([0-9]+):\s([0-9]+)x([0-9]+)", data):
    i, x, y, w, h = [int(d) for d in match.groups()]
    fabric[y:y+h, x:x+w] += 1

print('# square inches within > 1 claims:', np.sum(fabric > 1))


# Part 2
# Do a second pass and check if the entire rectangle only has one
# claim for each square.

for match in re.finditer("#([0-9]+)\s@\s([0-9]+),([0-9]+):\s([0-9]+)x([0-9]+)", data):
    i, x, y, w, h = [int(d) for d in match.groups()]

    if np.sum(fabric[y:y+h, x:x+w]) == w * h:
        print("Non-overlapping claim id:", i)
