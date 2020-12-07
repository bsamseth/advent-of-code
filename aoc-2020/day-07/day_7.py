import re

with open("input.txt") as f:
    rules = {
        re.match(r"^\w+ \w+", line).group(0): re.findall(r"(\d+) (\w+ \w+)", line)
        for line in f
    }


def can_hold(target_color):
    for color, contains in rules.items():
        if any(sub_color == target_color for _, sub_color in contains):
            yield color
            yield from can_hold(color)


def bags_needed(color):
    return 1 + sum(
        int(count) * bags_needed(sub_color) for count, sub_color in rules[color]
    )


print("Part 1:", len(set(can_hold("shiny gold"))))
print("Part 2:", bags_needed("shiny gold") - 1)
