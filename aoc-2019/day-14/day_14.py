from dataclasses import dataclass
from collections import defaultdict
from typing import Dict, List
from pprint import pprint
from math import ceil


@dataclass
class Reactant:
    quantity: int
    ID: str


@dataclass
class Reaction:
    output: Reactant
    dependencies: List[Reactant]


# def produce(
#     reactant: Reactant, reactions: Dict[str, Reaction], available: Dict[str, int]
# ):
#     if reactant.ID not in available:
#         available[reactant.ID] = 0
#
#     if reactant.ID == "ORE":
#         # Make sure there's enough ore available, but only fill when necessary.
#         needed_units = max(reactant.quantity - available["ORE"], 0)
#         available["ORE"] += needed_units
#         return needed_units
#
#     # If we already have some of the reactant, we don't need to produce as much.
#     if available[reactant.ID] > 0:
#         reactant.quantity -= available[reactant.ID]
#
#         # If we actually had enough, there's no need to do anything.
#         if reactant.quantity <= 0:
#             return 0
#
#     producing_reaction = reactions[reactant.ID]
#     assert producing_reaction.output.ID == reactant.ID
#     produced_per_reaction = producing_reaction.output.quantity
#
#     ore_produced = 0
#     while available[reactant.ID] < reactant.quantity:
#         for dependency in producing_reaction.dependencies:
#             ore_produced += produce(dependency, reactions, available)
#             available[dependency.ID] -= dependency.quantity
#
#         available[reactant.ID] += produced_per_reaction
#
#     return ore_produced


def produce(reactant: Reactant, reactions: Dict[str, Reaction]):

    if reactant.ID not in reactions:
        return {reactant.ID: reactant.quantity}

    producing_reaction = reactions[reactant.ID]
    assert producing_reaction.output.ID == reactant.ID
    produced_per_reaction = producing_reaction.output.quantity
    reactions_needed = ceil(reactant.quantity / produced_per_reaction)

    stuff_needed: Dict[str, int] = defaultdict(int)
    for _ in range(reactions_needed):
        for dep in producing_reaction.dependencies:
            for k, v in produce(dep, reactions).items():
                stuff_needed[k] += v
    return stuff_needed


reactions = {}
with open("input-test2.txt") as f:
    for line in f:
        inp, out = line.strip().split("=>")

        result = Reactant(quantity=int(out.split()[0]), ID=out.split()[1])
        reactants = [
            Reactant(quantity=int(part.split()[0]), ID=part.split()[1])
            for part in inp.split(",")
        ]

        reactions[result.ID] = Reaction(result, reactants)


base_reactions = {
    k: v for k, v in reactions.items() if any(d.ID == "ORE" for d in v.dependencies)
}
nested_reactions = {k: v for k, v in reactions.items() if k not in base_reactions}
pprint(base_reactions)
pprint(nested_reactions)

stuff_needed = produce(Reactant(1, "FUEL"), nested_reactions)
final_ore = 0
for stuff, count in stuff_needed.items():
    ore_needed = produce(Reactant(ID=stuff, quantity=count), base_reactions)
    assert len(ore_needed) == 1 and "ORE" in ore_needed
    final_ore += ore_needed["ORE"]

print(final_ore)
