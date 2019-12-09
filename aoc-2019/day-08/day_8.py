import numpy as np

with open("input.txt") as f:
    data = np.asarray([int(a) for a in f.read().strip()])

width, height = 25, 6
data = data.reshape(-1, height, width)

fewest_zeros = np.argmin(np.sum(data == 0, axis=(1, 2)))
print("Part 1:", (data[fewest_zeros] == 1).sum() * (data[fewest_zeros] == 2).sum())

print("Part 2:")
for i in range(height):
    for j in range(width):
        for pixel in data[:, i, j]:
            if pixel != 2:
                print(u"\u2588" if pixel == 1 else " ", end="")
                break
    print()
