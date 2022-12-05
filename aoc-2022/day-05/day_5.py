import re
from collections import defaultdict
from typing import Literal

from aocd import data, submit


def parse_stacks(stack_desc: str) -> dict[int, list[str]]:
    stacks: dict[int, list[str]] = defaultdict(list)
    for line in stack_desc.splitlines()[:-1]:
        for i in range(0, len(line), 4):
            crate = line[i:i+4]
            if crate.strip():
               stacks[i//4+1].insert(0, crate[1]) 
    return stacks

def simulate_crane(stacks: dict[int, list[str]], procedure: str, crane_model: Literal[9000, 9001]) -> str:
    for step in procedure.splitlines():
        count, f, t = map(int, re.findall(r"\d+", step))

        if crane_model == 9000:
            for _ in range(count):
                stacks[t].append(stacks[f].pop())
        else:
            stacks[f], lift = stacks[f][:-count], stacks[f][-count:]
            stacks[t].extend(lift)

    return "".join(stack[-1] for _, stack in sorted(stacks.items()))




stack_desc, procedure = data.split("\n\n")
submit(part=1, answer=simulate_crane(parse_stacks(stack_desc), procedure, 9000))
submit(part=2, answer=simulate_crane(parse_stacks(stack_desc), procedure, 9001))
