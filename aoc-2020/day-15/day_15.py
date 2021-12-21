# Note: this broke when moving to Python 3.9 duo to some issue with numba.
# Refer to C++ implementation.

import time

import numpy as np
from numba import njit, uint32


@njit(uint32(uint32, uint32[:], uint32, uint32))
def play_till_round(max_round, memory, last_number, n_starting_numbers):
    for round_nr in range(n_starting_numbers + 1, max_round + 1):
        next_number = (
            round_nr - 1 - from_memory if (from_memory := memory[last_number]) else 0
        )
        memory[last_number] = round_nr - 1
        last_number = next_number
    return next_number


def initial_memory(numbers, max_round):
    memory = np.zeros(max_round, dtype=np.uint32)
    for i, n in enumerate(numbers[:-1]):
        memory[n] = i + 1
    return memory


with open("input.txt") as f:
    init_numbers = list(map(int, f.read().strip().split(",")))

t0 = time.time()
for part, n in ((1, 2020), (2, 30_000_000)):
    print(
        f"Part {part}:",
        play_till_round(
            n, initial_memory(init_numbers, n), init_numbers[-1], len(init_numbers)
        ),
    )
print(f"Total time: {round(time.time() - t0, 2)} seconds")
