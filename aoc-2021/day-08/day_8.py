from toolz import pipe
from toolz.curried import map

unique_count = 0
total = 0

with open("input.txt") as f:
    for line in f:
        first, second = line.split("|")
        patterns = ["".join(sorted(x)) for x in first.strip().split()]
        digits = ["".join(sorted(x)) for x in second.strip().split()]
        unique_count += sum(1 for digit in digits if len(digit) in [2, 3, 4, 7])

        one = set(next(pattern for pattern in patterns if len(pattern) == 2))
        seven = set(next(pattern for pattern in patterns if len(pattern) == 3))
        four = set(next(pattern for pattern in patterns if len(pattern) == 4))
        eight = set(next(pattern for pattern in patterns if len(pattern) == 7))
        nine, six, zero = sorted(
            [set(pattern) for pattern in patterns if len(pattern) == 6],
            key=lambda x: (not four.issubset(x), x & (four - one) != (four - one)),
        )

        f = one - six
        c = one - f

        three, five, two = sorted(
            [set(pattern) for pattern in patterns if len(pattern) == 5],
            key=lambda x: (bool(x & c), bool(x & f)),
            reverse=True,
        )

        def to_digit(x):
            return [zero, one, two, three, four, five, six, seven, eight, nine].index(
                set(x)
            )

        decoded = pipe(digits, map(to_digit), map(str), "".join, int)
        total += decoded


print("Part 1:", unique_count)
print("Part 2:", total)
