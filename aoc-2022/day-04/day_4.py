import re

from aocd import data, submit

contained = 0
overlapping= 0
for line in data.splitlines():
    f1, t1, f2, t2 = map(int, re.findall(r"\d+", line))
    f, t = max(f1, f2), min(t1, t2)
    if (f == f1 and t ==t1) or (f == f2 and t == t2):
        contained += 1
    if f2 <= t1 and f1 <= t2:
        overlapping += 1

submit(part=1, answer=contained)
submit(part=2, answer=overlapping)
