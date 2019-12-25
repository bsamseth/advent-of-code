import numpy as np
from numba import njit


def get_matrix(n):
    pattern = np.array([0, 1, 0, -1])
    matrix = np.zeros((n, n), dtype=int)
    for i in range(n):
        matrix[i] = np.roll(
            np.tile(np.repeat(pattern, i + 1), n // (pattern.size * (i + 1)) + 1), -1
        )[:n]
    return matrix


def iterate(A, x, times):
    for _ in range(times):
        x = np.abs(A @ x) % 10
    return x


@njit
def calc(x):
    for _ in range(100):
        s = 0
        for i in range(len(x) - 1, -1, -1):
            s += x[i]
            x[i] = s % 10
    return x


with open("input.txt") as f:
    vector = np.array([int(d) for d in list(f.read().strip())])

matrix = get_matrix(len(vector))
print("Part 1:", "".join(str(d) for d in iterate(matrix, vector, 100)[:8]))


offset = int("".join(str(d) for d in vector[:7]))
full_length = len(vector) * 10000
offset_modulus = offset % full_length
need_to_do = full_length - offset
new_vector = np.roll(vector, -offset_modulus)
result = calc(np.tile(new_vector, need_to_do // len(new_vector) + 1)[:need_to_do])
print("Part 2:", "".join(str(d) for d in result[:8]))
