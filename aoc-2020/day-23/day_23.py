import time

from toolz import sliding_window

t0 = time.time()


def move(curr, successors):
    a = successors[curr]
    b = successors[a]
    c = successors[b]
    after_c = successors[c]

    d = curr
    while (d := d - 1) in (a, b, c) or d == 0:
        if d == 0:
            d = len(successors)

    successors[curr] = after_c
    successors[c] = successors[d]
    successors[d] = a
    return after_c


def solve(order, moves):
    # Build successor array:
    successors = [0] * (len(order) + 1)
    for a, b in sliding_window(2, order):
        successors[a] = b
    successors[order[-1]] = order[0]

    # Perform moves:
    curr = order[0]
    for _ in range(moves):
        curr = move(curr, successors)
    return successors


def iterate_successors(successors):
    """Generate all elements in the order given by successors, starting at 1."""
    curr = 1
    yield 1
    while (curr := successors[curr]) != 1:
        yield curr


with open("input.txt") as f:
    starting_order = list(map(int, list(f.read().strip())))

print("Part 1:", "".join(map(str, iterate_successors(solve(starting_order, 100))))[1:])

successors = solve(
    starting_order + list(range(len(starting_order) + 1, 1_000_000 + 1)), 10_000_000
)
print("Part 2:", successors[1] * successors[successors[1]])
print(f"Total time: {round(time.time() - t0, 2)} seconds")
