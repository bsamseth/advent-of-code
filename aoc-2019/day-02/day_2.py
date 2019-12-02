import numpy as np


def run(prog_orig, a, b):
    prog = np.copy(prog_orig)  # Copy the use prog as const ref.
    prog[1:3] = a, b

    pos = 0
    try:
        while True:
            op, l, r, o = prog[pos : pos + 4]

            if op == 1:
                prog[o] = prog[l] + prog[r]
            elif op == 2:
                prog[o] = prog[l] * prog[r]
            elif op == 99:
                break
            else:
                assert "Bad opcode!"

            pos += 4

        return prog[0]
    except IndexError:
        # Invalid program, skip
        return -1


prog = np.loadtxt("input.txt", delimiter=",", dtype=int)

for a in range(100):
    for b in range(100):
        out = run(prog, a, b)

        if a == 12 and b == 2:
            print("Part 1:", out)
        if out == 19690720:
            print("Part 2:", 100 * a + b)
            break
