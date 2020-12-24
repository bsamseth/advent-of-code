from toolz import iterate, last

with open("input.txt") as f:
    starting_order = list(map(int, list(f.read().strip())))


def destination_cup(dest_value, cups):
    if dest_value < 0:
        return destination_cup(max(cups), cups)
    try:
        return cups.index(dest_value)
    except ValueError:
        return destination_cup(dest_value - 1, cups)


def move(order):
    curr, a, b, c, *rest = order
    dest_index = destination_cup(curr - 1, rest)
    new_order = rest[: dest_index + 1] + [a, b, c] + rest[dest_index + 1 :] + [curr]
    return new_order


def move_n(order, n):
    return last(last(zip(range(n + 1), iterate(move, order))))


def normalize_order(order):
    one = order.index(1)
    return order[one + 1 :] + order[:one]


print("Part 1:", "".join(map(str, normalize_order(move_n(starting_order, 100)))))
