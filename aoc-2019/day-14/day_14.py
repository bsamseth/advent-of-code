from collections import defaultdict
from dataclasses import dataclass
from math import ceil
from typing import Dict, List


@dataclass
class Reactant:
    quantity: int
    ID: str


@dataclass
class Reaction:
    output: Reactant
    dependencies: List[Reactant]


def ore_required_for(target: Reactant, reactions: Dict[str, Reaction]) -> int:
    """Return the units of ORE needed to produce the target."""
    ore = 0
    excess: Dict[str, int] = defaultdict(int)
    needed = [target]

    while needed:
        cur = needed.pop()

        if cur.ID == "ORE":
            ore += cur.quantity
            continue

        # Account for excess:
        take = min(excess[cur.ID], cur.quantity)
        excess[cur.ID] -= take
        cur.quantity -= take

        if not cur.quantity:
            continue

        producing_reaction = reactions[cur.ID]
        needed_reactions = ceil(cur.quantity / producing_reaction.output.quantity)
        excess[cur.ID] += (
            producing_reaction.output.quantity * needed_reactions - cur.quantity
        )
        for dep in producing_reaction.dependencies:
            needed.append(Reactant(ID=dep.ID, quantity=dep.quantity * needed_reactions))

    return ore


def binary_search(max_ore: int, lower: int, upper: int, reactions: Dict[str, Reaction]):
    """Return the max fuel possible to produce with the given ore supply."""

    assert lower < upper

    while lower + 1 < upper:
        mid = (lower + upper) // 2
        value = ore_required_for(Reactant(ID="FUEL", quantity=mid), reactions)

        if value > max_ore:
            upper = mid
        elif value < max_ore:
            lower = mid
        else:
            lower = upper = mid

    return lower


# Parse input:
reactions: Dict[str, Reaction] = {}
with open("input.txt") as f:
    for line in f:
        inp, out = line.strip().split("=>")

        result = Reactant(quantity=int(out.split()[0]), ID=out.split()[1])
        reactants = [
            Reactant(quantity=int(part.split()[0]), ID=part.split()[1])
            for part in inp.split(",")
        ]

        reactions[result.ID] = Reaction(result, reactants)

ore_to_fuel = ore_required_for(Reactant(ID="FUEL", quantity=1), reactions)
print("Part 1:", ore_to_fuel)

# For the binary search, ore / ore_per_fuel is a lower limit, and twice that is a solid upper limit.
ore_available = 1000000000000
print(
    "Part 2:",
    binary_search(
        max_ore=ore_available,
        lower=ore_available // ore_to_fuel,
        upper=ore_available // ore_to_fuel * 2,
        reactions=reactions,
    ),
)
