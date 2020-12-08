import re

with open("input.txt") as f:
    program = [(op, int(arg)) for op, arg in re.findall(r"(\w+) ([+-]\d+)", f.read())]


ops = {
    "acc": (lambda arg, ip, acc: (acc + arg, ip + 1)),
    "nop": (lambda arg, ip, acc: (acc, ip + 1)),
    "jmp": (lambda arg, ip, acc: (acc, ip + arg)),
}


def ex(prog, ip=0, acc=0, history=None):
    if history is None:
        history = set()
    if ip in history or ip < 0:
        return False, acc
    if ip == len(prog):
        return True, acc
    op, arg = prog[ip]
    new_acc, new_ip = ops[op](arg, ip, acc)
    return ex(prog, new_ip, new_acc, history | {ip})


print("Part 1:", ex(program)[1])

for i, (op, arg) in enumerate(program):
    if op != "acc":
        new_op = "nop" if op == "jmp" else "nop"
        p = program[:]
        p[i] = (new_op, arg)
        success, acc = ex(p)
        if success:
            print("Part 2:", acc)
            break
