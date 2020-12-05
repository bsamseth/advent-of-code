"""
The helpful observation is that the boarding pass "code" spells out the seat id
in binary if we treat B and R as 1, and F and L as 0.
"""

from toolz import reduce, sliding_window, first, filter


def seat_id(boarding_pass: str) -> int:
    return reduce(lambda n, c: (n << 1) + (c in "BR"), boarding_pass, 0)


with open("input.txt") as f:
    seat_ids = sorted(map(seat_id, map(str.rstrip, f)))

print("Part 1:", seat_ids[-1])  # Max seat id.
print(
    "Part 2:",  # First missing seat id greater than the first seat id.
    first(first(filter(lambda w: w[0] != w[1] - 1, sliding_window(2, seat_ids)))) + 1,
)
