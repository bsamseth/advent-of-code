import re
import time
from collections import deque
from enum import Enum, unique, auto
from pprint import pprint

with open("input.txt") as f:
    data = f.read().strip()

clay = set()
ylim = (float("inf"), 0)
xlim = (float("inf"), 0)
for line in data.split("\n"):
    m = re.match(
        r"""
    (^x=(?P<xbase>[0-9]+),\s+y=(?P<ymin>[0-9]+)\.\.(?P<ymax>[0-9]+))?  # or,
    (^y=(?P<ybase>[0-9]+),\s+x=(?P<xmin>[0-9]+)\.\.(?P<xmax>[0-9]+))?
    """,
        line,
        flags=re.VERBOSE,
    )
    _, x, ymin, ymax, _, y, xmin, xmax = m.groups()
    if x is not None:
        x, ymin, ymax = (int(d) for d in (x, ymin, ymax))
        ylim = min(ylim[0], ymin), max(ylim[1], ymax)
        xlim = min(xlim[0], x), max(xlim[1], x)
        clay.update({(y, x) for y in range(ymin, ymax + 1)})
    else:
        y, xmin, xmax = (int(d) for d in (y, xmin, xmax))
        ylim = min(ylim[0], y), max(ylim[1], y)
        xlim = min(xlim[0], xmin), max(xlim[1], xmax)
        clay.update({(y, x) for x in range(xmin, xmax + 1)})


wet = set()
still = set()


def draw():
    s = ""
    for y in range(0, max(ylim) + 2):
        for x in range(min(xlim) - 1, max(xlim) + 2):
            if (y, x) == (0, 500):
                s += "+"
            elif (y, x) in clay:
                s += "#"
            elif (y, x) in still:
                s += "~"
            elif (y, x) in wet:
                s += "|"
            else:
                s += "."
        s += "\n"
    print(s)


@unique
class DropType(Enum):
    INF_FALL = auto()
    FALL = auto()
    BLOCKED = auto()


def drop(y, x, yprev, xprev, blocked=False):
    wet.add((y, x))

    if y + 1 > max(ylim):
        return DropType.INF_FALL

    if (y + 1, x) not in clay and (y + 1, x) not in still:
        # Fall down.
        return drop(y + 1, x, y, x)

    else:
        result_left, result_right = None, None

        if (yprev, xprev) != (y, x - 1):
            # Flow left?
            blocked_right = blocked
            if not blocked_right:
                for i in range(1, max(xlim) - x + 1):
                    if (y + 1, x + i) not in (clay | still):
                        break
                    if (y, x + i) in clay:
                        blocked_right = True
                        break

            blocked_left = (y, x - 1) in clay
            if blocked_left:
                if blocked_right:
                    still.add((y, x))
                result_left = DropType.BLOCKED
            else:
                result_left = drop(y, x - 1, y, x, blocked=blocked_right)
                if result_left == DropType.BLOCKED and blocked_right:
                    still.add((y, x))

        if (yprev, xprev) != (y, x + 1):
            # Flow right?
            blocked_left = blocked
            if not blocked_left:
                for i in range(1, min(xlim) + x + 1):
                    if (y + 1, x - i) not in (clay | still):
                        break
                    if (y, x + i) in clay:
                        blocked_left = True
                        break

            blocked_right = (y, x + 1) in clay
            if blocked_right:
                if blocked_left:
                    still.add((y, x))
                result_right = DropType.BLOCKED
            else:
                result_right = drop(y, x + 1, y, x, blocked=blocked_left)
                if result_right == DropType.BLOCKED and blocked_left:
                    still.add((y, x))

        if result_left is not None:
            return result_left
        else:
            return result_right


y, x = 0, 500
while y < min(ylim) and (y + 1, x) not in clay:
    y += 1

draw()
n_wet = len(wet)
while True:
    drop(y, x, y - 1, x)

    if n_wet == len(wet):
        break
    n_wet = len(wet)
draw()
print(n_wet)
