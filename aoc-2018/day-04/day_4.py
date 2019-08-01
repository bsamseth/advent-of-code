import datetime
from pprint import pprint
from collections import namedtuple, defaultdict
import re
import numpy as np


# Read data into list of entries on the format (date, comment):
Entry = namedtuple("Entry", "date comment")

with open("input1.txt", "r") as f:
    data = []
    for line in f.read().strip().split("\n"):
        datestr, comment = re.match("\[([0-9\- :]+)\] (.*)", line).groups()
        data.append(
            Entry(datetime.datetime.strptime(datestr, "%Y-%m-%d %H:%M"), comment)
        )

# Part 1
#
# Accumulate the minutes slept for each guard. The dict is
# indexed by guard id, and contains a len-60 array with one element
# for each minute of the midnight hour. Each element is the number
# of times minute i was slept during.
guards = dict()
guard_on_shift = None
asleep_since = None
for entry in sorted(data):
    match = re.match(
        "((Guard #(?P<guard_id>[0-9]+))|(?P<wakes_up>wakes up)|(?P<falls_asleep>falls asleep))",
        entry.comment,
    )
    if match.group("guard_id"):
        guard_on_shift = int(match.group("guard_id"))
        if not guard_on_shift in guards:
            guards[guard_on_shift] = np.zeros(60, dtype=int)
    elif match.group("falls_asleep"):
        asleep_since = entry.date.minute
    elif match.group("wakes_up"):
        guards[guard_on_shift][asleep_since : entry.date.minute] += 1
    else:
        assert False, "Badly formatted entry comment: {}".format(entry)

# Pick the guard with the most minutes sleps in total, the the minute
# that contributed the most.
sleepy_guard = max(guards, key=lambda g: np.sum(guards[g]))
sleepy_minute = np.argmax(guards[sleepy_guard])
print(
    "Strategy 1: Guard #{} x minute {} = {}".format(
        sleepy_guard, sleepy_minute, sleepy_guard * sleepy_minute
    )
)


# Part 2.
#
# Pick the guard based on who has the largest count for any single minute.
# Only difference is to select from the dict based on np.max and not np.sum.
sleepy_guard = max(guards, key=lambda g: np.max(guards[g]))
sleepy_minute = np.argmax(guards[sleepy_guard])
print(
    "Strategy 2: Guard #{} x minute {} = {}".format(
        sleepy_guard, sleepy_minute, sleepy_guard * sleepy_minute
    )
)
