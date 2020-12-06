import operator
from toolz import compose, map, reduce

with open("input.txt") as f:
    groups = f.read().split("\n\n")


print(
    "Part 1:", sum(map(compose(len, set, list, lambda g: g.replace("\n", "")), groups))
)

print(
    "Part 2:",
    sum(
        len(reduce(operator.and_, map(set, group)))
        for group in map(lambda g: g.split("\n"), groups)
    ),
)
