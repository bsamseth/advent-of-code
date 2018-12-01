import numpy as np
from itertools import count

# Part one
data = np.loadtxt("input1.txt", dtype=int)
print("Final frequency:", np.sum(data))

# Part two
seen = set([0])
freq = 0
for index in count():
    freq += data[index % len(data)]
    if freq in seen:
        break
    seen.add(freq)

print("First frequency reached twice:", freq)
print("Found in {} iterations".format(index))
