from toolz import memoize, concatv, frequencies, drop

with open("input.txt") as f:
    in_bag = sorted(map(int, f))

adapters = tuple(concatv([0], in_bag, [in_bag[-1] + 3]))
counts = frequencies(map(lambda x: x[1] - x[0], zip(adapters, drop(1, adapters))))
print("Part 1:", counts[1] * counts[3])


@memoize
def ways(adapters):
    if len(adapters) <= 2:
        return 1

    start, rest = adapters[0], adapters[1:]
    return sum(
        ways(r) for r in map(lambda n: rest[n:], range(len(rest))) if r[0] - 3 <= start
    )


print("Part 2:", ways(adapters))
