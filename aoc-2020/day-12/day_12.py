def step(part, instruction, pos, dir):
    c, n = instruction
    if c in "FNEWS":
        change = {"N": 1j, "E": 1, "W": -1, "S": -1j, "F": dir}[c] * n
        if c == "F" or part == 1:
            return pos + change, dir
        else:
            return pos, dir + change
    else:
        return pos, dir * (1j - 2j * (c == "R")) ** (n // 90)


def dist_to_endpoint(part, steps, pos, dir):
    if not steps:
        return int(abs(pos.real) + abs(pos.imag))
    return dist_to_endpoint(part, steps[1:], *step(part, steps[0], pos, dir))


with open("input.txt") as f:
    instructions = [(line[0], int(line[1:])) for line in f]

print("Part 1:", dist_to_endpoint(part=1, steps=instructions, pos=0, dir=1))
print("Part 2:", dist_to_endpoint(part=2, steps=instructions, pos=0, dir=10 + 1j))
