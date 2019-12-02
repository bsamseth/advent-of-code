import numpy as np

data = np.loadtxt('input.txt', dtype=int)

print("Part 1:", np.sum(data // 3 - 2))

total_fuel = 0

for mass in data:
    while True:
        fuel_needed = mass // 3 - 2

        if fuel_needed <= 0:
            break

        total_fuel += fuel_needed
        mass = fuel_needed


print("Part 2:", total_fuel)
