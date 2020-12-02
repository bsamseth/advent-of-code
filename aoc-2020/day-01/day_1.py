with open("input.txt") as f:
    expenses = [int(line) for line in f]


def part1():
    pairs = set()
    for n in expenses:
        if n in pairs:
            print(f"Part 1: {n * (2020 - n)}")
            return
        pairs.add(2020 - n)


def part2():
    triplets = {}
    for i, n in enumerate(expenses):
        for m in expenses[i + 1 :]:
            if m in triplets:
                a, b = triplets[m]
                print(f"Part 2: {a*b*m}")
                return
            triplets[2020 - n - m] = (n, m)


part1()
part2()
