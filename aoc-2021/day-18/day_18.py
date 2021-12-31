import re
from functools import partial
from itertools import product

import toolz
from aocd import data, submit


def explode(number, start, stop):
    def repl(add, flip, match):
        m = match.group()[:: -1 if flip else 1]
        r = str(int(m) + add)[:: -1 if flip else 1]
        return r

    left_part, right_part = eval(number[start : stop + 1])

    right = re.sub(
        r"\d+", partial(repl, right_part, False), number[stop + 1 :], count=1
    )
    left = re.sub(
        r"\d+", partial(repl, left_part, True), number[:start][::-1], count=1
    )[::-1]
    bang = left + "0" + right
    return bang


def reduce_step(number):
    depth = 0
    for i, c in enumerate(number):
        if c == "[":
            depth += 1
        elif c == "]":
            depth -= 1
        if depth > 4:
            return explode(number, i, i + number[i:].index("]"))
    for i, c in enumerate(number):
        if m := re.match(r"\d\d+", number[i:]):
            left = number[:i]
            right = number[i + len(m.group()) :]
            d, m = divmod(int(m.group()), 2)
            split = left + f"[{d},{d+m}]" + right
            return split
    return number


def reduce(number):
    number = number.replace(" ", "")
    while (new_number := reduce_step(number)) != number:
        number = new_number
    return number


def add(a, b):
    return reduce(f"[{a},{b}]")


def magnitude(number_str):
    def recr(number):
        if isinstance(number, int):
            return number
        return 3 * recr(number[0]) + 2 * recr(number[1])

    return recr(eval(number_str))


submit(magnitude(toolz.reduce(add, data.splitlines())), part=1)
submit(
    max(magnitude(add(a, b)) for a, b in product(data.splitlines(), repeat=2)),
    part=2,
)
