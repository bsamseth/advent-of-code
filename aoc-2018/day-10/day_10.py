import re
import numpy as np
from matplotlib import pyplot as plt
from matplotlib import animation


"""
# To find wait time, run animation manually and search for where the points are closest together.

def update_plot(i):
    print('called', i)
    global x, y
    x += vx
    y += vy
    scat.set_offsets(np.c_[x, y])
    return scat,

def init_plot():
    scat = ax.scatter([], [], animated=True)
    xmin, xmax = np.min(x), np.max(x)
    ymin, ymax = np.min(y), np.max(y)
    ax.axis([xmin, xmax, ymin, ymax])
    return scat,

fig, ax = plt.subplots()
scat, *_ = init_plot()
ani = animation.FuncAnimation(fig, update_plot, init_func=init_plot, frames=range(2), interval=1000, blit=True)
plt.show()
"""

with open("input.txt", "r") as f:
    data = [
        match.groups()
        for match in re.finditer(
            "<\s*([\-0-9]+),\s*([\-0-9]+)>.*<\s*([\-0-9]+),\s*([\-0-9]+)>", f.read()
        )
    ]

x, y, vx, vy = np.asarray(data, dtype=int).T
y, vy = -y, -vy  # Invert y-axis

wait_time = 10054
x += wait_time * vx
y += wait_time * vy
plt.scatter(x, y)
plt.axis('equal')
print(f"Message appears after {wait_time} seconds")

plt.show()
