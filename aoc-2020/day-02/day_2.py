import re

with open("input.txt") as f:
    entries = [re.match(r"(\d+)+-(\d+) (\w): (\w+)", line).groups() for line in f]

valid_count_1 = 0
valid_count_2 = 0
for entry in entries:
    low, hi = map(int, entry[:2])
    char, password = entry[2:]
    valid_count_1 += low <= list(password).count(char) <= hi
    valid_count_2 += (password[low - 1] == char) ^ (password[hi - 1] == char)

print(f"Part 1: {valid_count_1}")
print(f"Part 2: {valid_count_2}")
