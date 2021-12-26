import math
from collections import defaultdict

from aocd import data, submit
from toolz import sliding_window

template, _, *pairs = data.splitlines()

pairs = dict(map(lambda x: x.split(" -> "), pairs))
pair_counts = defaultdict(int)
for x, y in sliding_window(2, template):
    pair_counts[x + y] += 1

for i in range(1, 41):
    for (x, y), count in pair_counts.copy().items():
        pair_counts[x + y] -= count
        pair_counts[x + pairs[x + y]] += count
        pair_counts[pairs[x + y] + y] += count

    if i in (10, 40):
        char_counts = defaultdict(int)
        for (x, y), count in pair_counts.items():
            char_counts[x] += count
            char_counts[y] += count
        for k in char_counts:
            char_counts[k] = math.ceil(char_counts[k] / 2)
        least, *_, most = sorted(char_counts.items(), key=lambda x: x[1])
        submit(most[1] - least[1], part=(i == 40) + 1)
