import re
from itertools import takewhile

def bound_state(state, center):
    """
    Make sure there are 4 empty pots on either end of the state.
    This ensures all patterns are included in the state.

    Need to keep track of the potentially new index of the starting pot.
    """
    left_empties = min(4, sum( 1 for _ in takewhile(lambda pot: pot == '.', state)))
    right_empties = min(4, sum( 1 for _ in takewhile(lambda pot: pot == '.', state[::-1])))
    for _ in range(0, 4 - left_empties):
        state.insert(0, '.')
    for _ in range(0, 4 - right_empties):
        state.append('.')
    return state, center + 4 - left_empties

def pot_sum(state, center):
    """Return the sum of indices of pots with plants."""
    return sum(i - center for i, c in enumerate(state) if c == '#')


with open('input.txt', 'r') as f:
    initial = [c for c in f.readline().strip().split()[-1]]
    mapping = {match.group(1) : match.group(2) for match in re.finditer('([#.]{5}) => ([#.])', f.read())}

# Simulate forward.
state, center = initial, 0
gen = []
for generation in range(1, 200):  # Enough gens to converge.
    state, center = bound_state(state, center)
    next_state = state[:]
    for pot in range(2, len(state)-2):
        next_state[pot] = mapping[''.join(state[pot-2:pot+3])]
    state = next_state
    gen.append((generation, pot_sum(state, center)))

# Part 1.
print('Sum of numbers of pots with plants after 20 generations:', gen[20-1][1])

# Part 2.
# Pot numbers function converges to a linear function as gen -> inf
# Calculate pot_sum(gen) = a * gen + b
a = gen[-1][1] - gen[-2][1]
b = gen[-1][1] - gen[-1][0] * a

large_gen = 50000000000
print(f'Sum of numbers of pots with plants after {large_gen} generations:', a * large_gen + b)

