from string import ascii_lowercase

with open("input.txt", "r") as f:
    data = f.read().strip()

# Part 1
def react(polymer):
    result = []
    for unit in polymer:
        if result and unit.swapcase() == result[-1]:
            result.pop()
        else:
            result.append(unit)
    return len(result)


print("Polymer contains {} units after fully reacting.".format(react(data)))


# Part 2
shortest = float("inf")
for char in ascii_lowercase:
    removed = data.replace(char, "").replace(char.upper(), "")
    shortest = min(shortest, react(removed))

print("Shortest possible polymer contains {} units.".format(shortest))
