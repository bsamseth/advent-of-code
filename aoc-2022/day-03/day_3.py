from aocd import data, submit

priorities = {chr(ord("a") + i): i + 1 for i in range(26)} | {
    chr(ord("A") + i): i + 27 for i in range(26)
}
lines = data.splitlines()

sum_1 = 0
for line in lines:
    first, second = line[: len(line) // 2], line[len(line) // 2 :]
    dup = set(first).intersection(second).pop()
    sum_1 += priorities[dup]

sum_2 = 0
for i in range(len(lines) // 3):
    first, second, third = lines[3 * i : 3 * i + 3]
    dup = set(first).intersection(second).intersection(third).pop()
    sum_2 += priorities[dup]

submit(part=1, answer=sum_1)
submit(part=2, answer=sum_2)
