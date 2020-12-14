def addresses(mask, value):
    value = (value | int(mask.replace("X", "0"), 2)) & int(
        mask.replace("0", "1").replace("X", "0"), 2
    )
    floats = [i for i, bit in enumerate(reversed(mask)) if bit == "X"]
    for selection in range(2 ** len(floats)):
        float_selection = sum(
            ((1 << j) & selection) and (1 << i) for j, i in enumerate(floats)
        )
        yield float_selection | value


with open("input.txt") as f:
    program = f.read().splitlines()

mem_1 = {}
mem_2 = {}
for line in program:
    if line.startswith("mask"):
        mask = line.split("=")[-1]
        keep_mask = int(mask.replace("1", "0").replace("X", "1"), 2)
        set_mask = int(mask.replace("X", "0"), 2)
    else:
        where, value = line.split("=")
        where, value = int(where[4:-2]), int(value)
        mem_1[where] = (keep_mask & value) | set_mask

        for mem in addresses(mask, where):
            mem_2[mem] = value

print("Part 1:", sum(mem_1.values()))
print("Part 2:", sum(mem_2.values()))
