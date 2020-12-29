with open("input.txt") as f:
    pub_1, pub_2 = map(int, f.read().splitlines())

mod = 20201227
loop_size, value, pub = 0, 1, None
while not pub:
    value = (value * 7) % mod
    loop_size += 1

    if value == pub_1:
        pub = pub_2
    elif value == pub_2:
        pub = pub_1

value = 1
for _ in range(loop_size):
    value = (value * pub) % mod
print("Part 1:", value)
print("Part 2: No part 2!")
