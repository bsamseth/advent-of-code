import re
from copy import deepcopy
from dataclasses import dataclass
from functools import reduce
from operator import attrgetter, mul
from typing import Callable

from aocd import data, submit


@dataclass
class Monkey:
    id_: int
    items: list[int]
    operation: Callable[[int], int]
    divisible_test_by: int
    target_monkeys: tuple[int, int]
    inspections: int = 0

    @classmethod
    def from_str(cls, s) -> "Monkey":
        id_, items, operation, div, *t = s.splitlines()

        numbers = list(map(int, re.findall(r"\d+", "\n".join((id_, items, div, *t)))))
        op = next(re.finditer(r"= (.*)", operation)).group(1)
        return cls(
            id_=numbers[0],
            items=numbers[1:-3],
            operation=eval(f"lambda old: {op}"),
            divisible_test_by=int(numbers[-3]),
            target_monkeys=(numbers[-2], numbers[-1]),
        )


def monkey_business(
    monkeys: dict[int, Monkey], rounds: int, worry_transform: Callable[[int], int]
) -> int:
    for _ in range(rounds):
        for monkey in monkeys.values():
            for item in monkey.items:
                monkey.inspections += 1
                worry_level = worry_transform(monkey.operation(item))
                target_monkey = monkey.target_monkeys[
                    worry_level % monkey.divisible_test_by != 0
                ]
                monkeys[target_monkey].items.append(worry_level)
            monkey.items.clear()

    return reduce(
        mul, sorted(map(attrgetter("inspections"), monkeys.values()), reverse=True)[:2]
    )


monkeys = {m.id_: m for m in map(Monkey.from_str, data.split("\n\n"))}
submit(
    part=1,
    answer=monkey_business(
        deepcopy(monkeys), rounds=20, worry_transform=lambda x: x // 3
    ),
)
mod = reduce(mul, (m.divisible_test_by for m in monkeys.values()))
submit(
    part=2,
    answer=monkey_business(monkeys, rounds=10_000, worry_transform=lambda x: x % mod),
)
