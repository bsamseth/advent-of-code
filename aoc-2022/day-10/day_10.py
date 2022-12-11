from aocd import data, submit


def signal_strength(cycle: int, x: int) -> int:
    if cycle in (20, 60, 100, 140, 180, 220):
        return cycle * x
    return 0


def render(cycle: int, x: int) -> None:
    pixel = (cycle - 1) % 40
    end = "\n" if pixel == 39 else ""
    fill = "█" if x - 1 <= pixel <= x + 1 else " "
    return fill + end


part_1 = 0
cycle = 1
x = 1
image = ""
for instruction in data.splitlines():
    part_1 += signal_strength(cycle, x)
    image += render(cycle, x)

    if instruction == "noop":
        cycle += 1
    else:
        cycle += 1
        part_1 += signal_strength(cycle, x)
        image += render(cycle, x)
        cycle += 1
        x += int(instruction[5:])

submit(part=1, answer=part_1)
submit(part=2, answer="RLEZFLGE")  # From the printed image.
print(image)
