from itertools import cycle
from collections import defaultdict, deque
import re


def play(n_players, largest_marble):
    players = defaultdict(int)
    circle = deque([0])

    # The "current" is at the end of the deque at all times.
    for marble, player in zip(range(1, largest_marble + 1), cycle(range(n_players))):
        if marble % 23 == 0:
            circle.rotate(7)
            players[player] += marble + circle.pop()
            circle.rotate(-1)
        else:
            circle.rotate(-1)
            circle.append(marble)
    return max(players.values())


with open("input.txt", "r") as f:
    n_players, largest_marble = [
        int(_) for _ in re.match("([0-9]+)[^0-9]*([0-9]+)", f.read()).groups()
    ]

print("Winning elf' score:", play(n_players, largest_marble))
print(
    "Winning elf' score with 100 times more marbles:",
    play(n_players, largest_marble * 100),
)
