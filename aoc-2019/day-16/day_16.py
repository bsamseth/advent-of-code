import numpy as np


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


with open("input.txt") as f:
    vector = np.array([int(d) for d in list(f.read().strip())])

matrix = get_matrix(len(vector))
print("Part 1:", "".join(str(d) for d in iterate(matrix, vector, 100)[:8]))
