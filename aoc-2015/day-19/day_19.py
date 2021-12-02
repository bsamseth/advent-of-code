import re


def count_replacements(replacements, molecule):
    new_molecules = set()
    for i, part in enumerate(molecule):
        for f, t in replacements:
            if f == part:
                new_molecules.add("".join(molecule[:i] + [t] + molecule[i + 1 :]))

    return len(new_molecules)


with open("input.txt") as f:
    reps_text, target = f.read().strip().split("\n\n")

# Split target molecule into elements, and replacements into parts.
target_joined = target.strip()
target_split = re.findall(r"[A-Z][a-z]?", target_joined)
repls = [tuple(line.split(" => ")) for line in reps_text.split("\n")]

print("Part 1:", count_replacements(repls, target_split))

# Numerous attempts at searches (dfs, bfs, A*, greedy) tried.
# Greedy doesn't converge for this input, and the others take forever.
# Finally stole the solution after reading about it. Apparently some people
# got lucky with their inputs and it worked with a search. This is the general
# solution.
#
# Insight 1: Do the search from the formula to e, rather than the other way.
# This bit was pretty clear and I had this for all the searches anyway.
#
# Insight: Replace `Rn` with `(`, `Y` with `,` and `Ar` with `)`. Then all
# replacements are on this form
#
#   A => BC | B(C) | B(C,D) | B(C,D,E)
#
# where A, B, C, D and E are elements (H, or Ca, or Mg etc.) and may or may not
# be unique.
#
# If all where of the first form, it would take N - 1 replacements, where N is the
# number of elements in the target molecule (still counting `(`, `)` and `,` as single elements).
# The `B(C)` pattern is still reduced in a single step, meaning the `(` and the `)` are removed
# without any additional cost, and can therefore be subtracted from N. Going up to
# `B(C,D)` and `B(C,D,E)` we see that for each comma added we save two elements, i.e.
# the `,D` and `,D,E`. The final formula is then
#
#  N - 1 - (# of `(` or `)`) - 2 * (# of `,`)
#
print(
    "Part 2:",
    len(target_split)
    - target_split.count("Rn")
    - target_split.count("Ar")
    - 2 * target_split.count("Y")
    - 1,
)
