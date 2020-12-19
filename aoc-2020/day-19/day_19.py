import regex
from itertools import count

with open("input.txt") as f:
    rules, messages = f.read().strip().split("\n\n")
rules = {n: d for rule in rules.splitlines() for n, d in [rule.split(": ")]}
counter = count()  # Used to generated unique regex group names.


def build_regex(n, part):
    rule = rules[n]

    if match := regex.match(r'"(\w)"', rule):
        return match.group(1)
    if part == 2 and n == "8":
        return f"({build_regex('42', 2)}+)"
    if part == 2 and n == "11":
        a, b = build_regex("42", 2), build_regex("31", 2)
        name = f"r{next(counter)}"
        return f"(?P<{name}>{a}(?P>{name})?{b})"

    pattern = "|".join(
        "".join(build_regex(m, part) for m in sub_rule.split())
        for sub_rule in rule.split(" | ")
    )
    return f"({pattern})"


def count_matches(messages, part):
    return len(
        regex.findall(f"^{build_regex('0', part)}$", messages, flags=regex.MULTILINE)
    )


print("Part 1:", count_matches(messages, part=1))
print("Part 2:", count_matches(messages, part=2))
