import re


def manhattan(z):
    return int(abs(z.real) + abs(z.imag))


def trace_path(line):
    headings = {"R": 1, "U": 1j, "D": -1j, "L": -1}
    positions = set()
    steps_to_position = {}
    pos = steps = 0

    for match in re.finditer(r"([RLUD])(\d+)", line):

        c, n = match.groups()
        heading = headings[c]

        for _ in range(int(n)):
            pos += heading
            positions.add(pos)
            steps += 1

            if pos not in steps_to_position:
                steps_to_position[pos] = steps

    return positions, steps_to_position


with open("input.txt") as f:
    (p1, s1), (p2, s2) = [trace_path(line) for line in f]

intersection = p1 & p2

closest_intersection = min(intersection, key=manhattan)
print("Part 1:", manhattan(closest_intersection))

# Find the minimum sum of the steps along each path to an intersection.
print("Part 2:", min(s1[p] + s2[p] for p in intersection))
