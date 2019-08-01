from itertools import cycle

with open("input.txt", "r") as f:
    data = f.read()

m = {}
carts = {}
dir_d = {"<": -1, ">": 1, "^": 1j, "v": -1j}
for i, row in enumerate(data.split("\n")):
    for j, c in enumerate(row):
        loc = j - i * 1j
        if c in r"/\+":
            m[loc] = c
        elif c in dir_d:
            carts[loc] = dir_d[c], cycle([1j, 1, -1j])

while len(carts) > 1:
    for loc in sorted(carts, key=lambda x: (-x.imag, x.real)):
        if loc not in carts:
            continue  # deleted due to collision
        dxn, turn = carts.pop(loc)  # take out cart
        loc += dxn  # update position

        if loc in carts:  # handle collision
            print("collision!", f"{int(loc.real)},{int(-loc.imag)}")
            del carts[loc]
            continue

        track = m.get(loc)  # update direction
        if track == "+":
            dxn = dxn * next(turn)
        elif track is not None:  # / or \
            dxn *= 1j * (2 * ((track == "/") ^ (dxn.real == 0)) - 1)

        carts[loc] = dxn, turn  # put cart back onto tracks

pos, *_ = list(carts)
print(f"Final cart position: {int(pos.real)},{-int(pos.imag)}")
