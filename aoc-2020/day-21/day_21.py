import re
import z3
from itertools import product
from collections import Counter

with open("input.txt") as f:
    lines = [
        (m.group("ingredients").split(), m.group("allergens").split(", "))
        for m in re.finditer(
            r"(?P<ingredients>(\w+ )+)\(contains (?P<allergens>(\w+(, )?)+)\)", f.read()
        )
    ]

ingredients = {i for food, _ in lines for i in food}
allergens = {a for _, baz in lines for a in baz}
variables = {z3.Bool(f"{ing}_{alg}") for ing, alg in product(ingredients, allergens)}

eqs = []
# For every allergen in each food, one of the ingredients must have the allergen.
for line in lines:
    for allergen in line[1]:
        eqs.append(z3.Or(*[z3.Bool(f"{ing}_{allergen}") for ing in line[0]]))

# Every allergen is only found in one ingredient.
for allergen in allergens:
    eqs.append(
        z3.Sum([z3.If(z3.Bool(f"{ing}_{allergen}"), 1, 0) for ing in ingredients]) == 1
    )

# Every ingredient has at most one allergen.
for ing in ingredients:
    eqs.append(
        z3.Sum([z3.If(z3.Bool(f"{ing}_{allergen}"), 1, 0) for allergen in allergens])
        <= 1
    )

solver = z3.Solver()
solver.add(eqs)
solver.check()
model = solver.model()
ingredients_with_allergens = {
    ing: alg for v in variables for ing, alg in [repr(v).split("_")] if model.eval(v)
}
counter = Counter(ing for ings, _ in lines for ing in ings)
print(
    "Part 1:",
    sum(counter[i] for i in ingredients.difference(ingredients_with_allergens)),
)
print(
    "Part 2:",
    ",".join(
        sorted(
            ingredients_with_allergens, key=lambda ing: ingredients_with_allergens[ing]
        )
    ),
)
