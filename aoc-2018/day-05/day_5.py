from string import ascii_lowercase
from mpi4py import MPI

with open("input.txt", "r") as f:
    data = f.read().strip()


# Part 1
def react(polymer):
    i = 0
    while i < len(polymer) - 1:

        # Lowercase - uppercase differs by 32 in ASCII
        if abs(ord(polymer[i]) - ord(polymer[i + 1])) == 32:
            polymer = polymer[:i] + polymer[i + 2 :]
            i = max(0, i - 1)  # Backtrack one step to account for new possible match.

        else:
            i += 1

    return len(polymer)

print("Polymer contains {} units after fully reacting.".format(react(data)))


# Part 2
shortest = float('inf')
for char in ascii_lowercase:
    removed = data.replace(char, "").replace(char.upper(), "")
    shortest = min(shortest, react(removed))

print("Shortest possible polymer contains {} units.".format(shortest))

