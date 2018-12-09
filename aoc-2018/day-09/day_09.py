from itertools import cycle
import re


class Marble(object):
    def __init__(self, value, left=None, right=None):
        self.value = value
        self.left = left if left else self
        self.right = right if right else self

    def place_between(self, left, right):
        self.left = left
        self.right = right
        left.right = self
        right.left = self
        return self

    def remove(self):
        self.left.right = self.right
        self.right.left = self.left


class Circle(object):
    def __init__(self):
        self.current = Marble(0)

    def place_marble(self, value):
        if value % 23 != 0:
            self.current = Marble(value).place_between(
                self.current.right, self.current.right.right
            )
            return 0

        marble = self.current
        for _ in range(7):
            marble = marble.left
        self.current = marble.right
        marble.remove()
        return value + marble.value


def play(n_players, largest_marble):
    players = {p: 0 for p in range(1, n_players + 1)}
    circle = Circle()
    for marble, player in zip(range(1, largest_marble + 1), cycle(players)):
        players[player] += circle.place_marble(marble)
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
