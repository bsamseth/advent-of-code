from aocd import data, submit

elves = sorted(sum(map(int, elf.split())) for elf in data.split("\n\n"))

submit(part=1, answer=elves[-1])
submit(part=2, answer=sum(elves[-3:]))
