from io import StringIO

import matplotlib.pyplot as plt
import numpy as np

example = StringIO(
    """2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5"""
)
droplets = np.loadtxt("input.txt", dtype=int, delimiter=",")

# find the bounding box of the droptlets
min_x = np.min(droplets[:, 0]) - 1
max_x = np.max(droplets[:, 0]) + 1
min_y = np.min(droplets[:, 1]) - 1
max_y = np.max(droplets[:, 1]) + 1
min_z = np.min(droplets[:, 2]) - 1
max_z = np.max(droplets[:, 2]) + 1

# plot the bounding box
fig = plt.figure()
ax = fig.add_subplot(111, projection="3d")
ax.scatter(droplets[:, 0], droplets[:, 1], droplets[:, 2])
ax.set_aspect("equal")
plt.show()

print(f"Bounding box: {min_x}, {max_x}, {min_y}, {max_y}, {min_z}, {max_z}")
# print the volume of the bounding box
print(f"Volume: {(max_x - min_x) * (max_y - min_y) * (max_z - min_z)}")
