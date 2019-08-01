import numpy as np
import numba


def fuel_level(x, y, serial_number=0):
    """Calculate fuel level for the given coordinate(s)."""
    rack_id = x + 10
    return (((rack_id * y + serial_number) * rack_id) // 100) % 10 - 5


@numba.njit
def compute_partial_sum(grid):
    partial = np.copy(grid)
    for i in range(partial.shape[0]):
        for j in range(partial.shape[1]):
            if i > 0:
                partial[i, j] += partial[i - 1, j]
            if j > 0:
                partial[i, j] += partial[i, j - 1]
            if i > 0 and j > 0:
                partial[i, j] -= partial[i - 1, j - 1]
    return partial


# No njit due to creation and returning of arrays. But
# jit handles this so that it can njit the loops still, and
# just jiting the parts around. Major (!) speed boost.
@numba.autojit
def find_optimal_corners(grid):
    partial = compute_partial_sum(grid)
    grid_size = len(grid)
    maxes = -np.ones(grid_size + 1, dtype=int) * 5 * grid_size * (grid_size + 1)
    corners = np.empty((grid_size + 1, 2), dtype=int)
    for size in range(1, grid_size + 1):
        for i in range(partial.shape[0] - size):
            for j in range(partial.shape[1] - size):
                square_sum = partial[i + size - 1, j + size - 1]
                if i > 0:
                    square_sum -= partial[i - 1, j + size - 1]
                if j > 0:
                    square_sum -= partial[i + size - 1, j - 1]
                if i > 0 and j > 0:
                    square_sum += partial[i - 1, j - 1]
                if square_sum > maxes[size]:
                    maxes[size] = square_sum
                    corners[size] = j + 1, i + 1
    return maxes, corners


if __name__ == "__main__":

    serial_number = np.loadtxt("input.txt", dtype=int)
    x = np.arange(1, 301, dtype=int)
    x, y = np.meshgrid(x, x)

    # This is the 300 x 300 fuel grid.
    grid = fuel_level(x, y, serial_number=serial_number)

    maxes, corners = find_optimal_corners(grid)
    print("Max corner for 3x3 grid:", corners[3])
    max_size = np.argmax(maxes)
    print("Max corner for optimal grid:", corners[max_size], max_size)
