import numpy as np

def fuel_level(x, y, serial_number=0):
    rack_id = x + 10
    return (((rack_id * y + serial_number) * rack_id) // 100) % 10 - 5

def largest_nxn(grid, n):
    size = len(grid)
    max_corner = None, None
    max_fuel = -float('inf')
    for i in range(size - n - 1):
        for j in range(size - n - 1):
            fuel = np.sum(grid[i:i+n, j:j+n])
            if fuel > max_fuel:
                max_fuel = fuel
                max_corner = j+1, i+1
    return max_fuel, max_corner

serial_number = np.loadtxt('input.txt', dtype=int)
x = np.arange(1, 301, dtype=int)
x, y = np.meshgrid(x, x)
grid = fuel_level(x, y, serial_number=serial_number)

# Part 1
largest_total_power, corner = largest_nxn(grid, 3)
print('Top left corner of max 3x3 grid:', corner)


# Part 2
max_fuel = -float('inf')
for n in range(1, 301):
    fuel, corner = largest_nxn(grid, n)
    if fuel > max_fuel:
        max_fuel = fuel
        max_corner = corner
        n_max = n

print(f'Top left corner of max {n_max}x{n_max} grid:', max_corner)
