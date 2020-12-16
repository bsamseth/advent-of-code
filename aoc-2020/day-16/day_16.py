import re

with open("input.txt") as f:
    notes = f.read().strip()

fields = [
    (c, int(a_1), int(a_2), int(b_1), int(b_2))
    for c, a_1, a_2, b_1, b_2 in re.findall(
        r"((?:\w+ ?)+): (?:(\d+)-(\d+)) or (?:(\d+)-(\d+))", notes
    )
]
my_ticket, *nearby_tickets = [
    list(map(int, ticket.split(",")))
    for ticket in re.findall(r"^((?:\d+,?)+)$", notes, flags=re.MULTILINE)
]


def is_invalid(n):
    return not any(a <= n <= b or c <= n <= d for _, a, b, c, d in fields)


print("Part 1:", sum(n for ticket in nearby_tickets for n in ticket if is_invalid(n)))

valid_tickets = [
    ticket for ticket in nearby_tickets if not any(is_invalid(n) for n in ticket)
]
product = 1
columns = set(range(len(fields)))
for _ in range(len(fields)):
    for i, (field, a, b, c, d) in enumerate(fields):
        candidates = [
            col
            for col in columns
            if all(
                a <= ticket[col] <= b or c <= ticket[col] <= d
                for ticket in valid_tickets
            )
        ]
        if len(candidates) == 1:
            columns.remove(candidates[0])
            fields = fields[:i] + fields[i + 1 :]

            if field.startswith("departure"):
                product *= my_ticket[candidates[0]]
            break

print("Part 2:", product)
