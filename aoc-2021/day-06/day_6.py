from collections import Counter

with open("input.txt") as f:
    fish = Counter(int(x) for x in f.read().split(","))


def one_day(fish):
    new_fish = {k - 1: v for k, v in fish.items() if k > 0} | {8: fish.get(0, 0)}
    new_fish[6] = fish.get(7, 0) + fish.get(0, 0)
    return new_fish


for i in range(256):
    fish = one_day(fish)
    if i == 79:
        print("Part 1:", sum(fish.values()))

print("Part 2:", sum(fish.values()))
