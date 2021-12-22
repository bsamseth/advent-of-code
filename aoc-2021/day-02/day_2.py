import operator
from itertools import starmap

from toolz import accumulate


def parse_instruction(instruction):
    action, value = instruction.split()
    value = int(value)
    return (
        (value, 0)
        if action == "forward"
        else (0, value)
        if action == "down"
        else (0, -value)
    )


with open("input.txt") as f:
    x, y = zip(*map(parse_instruction, f))

horizontal, depth = map(sum, (x, y))
print("Part 1:", horizontal * depth)

depth = sum(starmap(operator.mul, zip(accumulate(operator.add, y), x)))
print("Part 2:", horizontal * depth)
