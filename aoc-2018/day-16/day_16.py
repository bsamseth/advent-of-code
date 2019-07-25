defs ={
    "addr": "r[A] + r[B]",
    "addi": "r[A] + B",
    "mulr": "r[A] * r[B]",
    "muli": "r[A] * B",
    "banr": "r[A] & r[B]",
    "bani": "r[A] & B",
    "borr": "r[A] | r[B]",
    "bori": "r[A] | B",
    "setr": "r[A]",
    "seti": "A",
    "gtir": "int(A    > r[B])",
    "gtri": "int(r[A] >    B)",
    "gtrr": "int(r[A] > r[B])",
    "eqir": "int(A    == r[B])",
    "eqri": "int(r[A] ==    B)",
    "eqrr": "int(r[A] == r[B])",
}

for name, body in defs.items():
    exec(f"""
def {name}(i, r):
    op, A, B, C = i
    r[C] = {body}
    return r""")

instructions = {name: eval(name) for name in defs}


with open("input.txt") as f:
    data = f.read().strip()
    part1, part2 = [part.strip() for part in data.split("\n\n\n")]


# Part 1:
options = {op: set(range(16)) for op in instructions}
three_count = 0
for sample in part1.split("\n\n"):
    first, second, third = sample.strip().split("\n")
    registers = eval(first.split(": ")[-1])
    instruction = eval(f"[{second.replace(' ', ',')}]")
    output = eval(third.split(": ")[-1])

    equal_count = 0
    for name, func  in instructions.items():
        out = func(instruction[:], registers[:])
        if out == output:
            equal_count += 1
        else:
            options[name] -= {instruction[0]}

    if equal_count >= 3:
        three_count += 1

print("Part 1:", three_count)


opcodes = {d: None for d in range(16)}
queue = {name for name in instructions if len(options[name]) == 1}
while queue:
    name = queue.pop()
    code = next(iter(options[name]))
    opcodes[code] = name

    for name in options:
        options[name] -= {code}
        if len(options[name]) == 1:
            queue.add(name)

registers = [0, 0, 0, 0]
for line in part2.split('\n'):
    instruction = [int(d) for d in line.split()]
    registers = instructions[opcodes[instruction[0]]](instruction, registers)

print("Part 2:", registers[0])

