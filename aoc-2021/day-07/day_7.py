import numpy as np

positions = np.loadtxt("input.txt", dtype=int, delimiter=",")

part1_fuel = np.abs(
    np.tile(positions, (positions.size, 1))
    - np.arange(1, positions.size + 1).reshape(-1, 1)
)
part2_fuel = part1_fuel * (part1_fuel + 1) // 2

print("Part 1:", part1_fuel.sum(axis=1).min())
print("Part 2:", part2_fuel.sum(axis=1).min())
