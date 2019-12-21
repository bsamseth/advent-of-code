from dataclasses import dataclass
from collections import defaultdict
from typing import Dict, List
from math import ceil


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


print("Part 1", ore_required_for(Reactant(ID="FUEL", quantity=1), reactions))
