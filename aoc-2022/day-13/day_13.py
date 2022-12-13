from functools import cmp_to_key
from itertools import zip_longest
from typing import Optional, Union

from aocd import data, submit

T = Optional[Union[int, list["T"]]]


def compare(left: T, right: T) -> int:
    if isinstance(left, int) and isinstance(right, list):
        left = [left]
    elif isinstance(left, list) and isinstance(right, int):
        right = [right]
    elif left is None:
        return -1
    elif right is None:
        return 1
    elif isinstance(left, int) and isinstance(right, int):
        return left - right

    for l, r in zip_longest(left, right):
        if cmp := compare(l, r):
            return cmp
    return 0


correctly_ordered = sum(
    i + 1
    for i, pair in enumerate(map(str.splitlines, data.split("\n\n")))
    if compare(*map(eval, pair)) < 0
)
submit(part=1, answer=correctly_ordered)

packets = sorted(
    list(map(eval, data.replace("\n\n", "\n").splitlines())) + [[[2]], [[6]]],
    key=cmp_to_key(compare),
)
submit(part=2, answer=(1 + packets.index([[2]])) * (1 + packets.index([[6]])))
