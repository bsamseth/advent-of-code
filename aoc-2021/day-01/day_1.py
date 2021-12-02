from toolz import sliding_window

with open("input.txt") as f:
    numbers = list(map(int, f.read().splitlines()))


def pair_compare(*x):
    return x[0] < x[1]


print(
    "Part 1:",
    sum(
        map(
            pair_compare,
            sliding_window(2, numbers),
        )
    ),
)
print(
    "Part 2:",
    sum(
        map(
            pair_compare,
            sliding_window(
                2, map(lambda z: sum(z) / len(z), sliding_window(3, numbers))
            ),
        )
    ),
)
