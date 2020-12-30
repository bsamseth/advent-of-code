import copy

from parse import parse


class Group:
    def __init__(self, army, desc):
        self.army = army
        parsed = parse(
            "{units:d} units each with {hp:d} hit points{paren}with an attack that does {damage:d} {damage_type} damage at initiative {initiative:d}",
            desc,
        )
        values = parsed.named
        values["weak"] = []
        values["immune"] = []
        if p := values["paren"].strip():
            for part in p.strip().lstrip("(").rstrip(")").split("; "):
                r = parse("{} to {}", part)
                values[r[0]] = r[1].split(", ")
        del values["paren"]
        self.__dict__.update(values)

    @property
    def effective_power(self):
        return self.units * self.damage

    def damage_against(self, enemy: "Group"):
        immune = self.damage_type in enemy.immune
        weak = self.damage_type in enemy.weak
        return int(not immune) * (1 + int(weak)) * self.effective_power


class Army:
    def __init__(self, desc: str):
        lines = desc.splitlines()
        self.name = lines[0][:-1]
        self.groups = list(map(lambda desc: Group(self, desc), lines[1:]))


def play_round(army1: Army, army2: Army):
    targets = []
    available_targets = {
        group for group in army1.groups + army2.groups if group.units > 0
    }
    for group in sorted(
        army1.groups + army2.groups,
        key=lambda g: (g.effective_power, g.initiative),
        reverse=True,
    ):
        if group.units == 0:
            continue

        enemies = {enemy for enemy in available_targets if enemy.army != group.army}

        if not enemies:
            continue

        damage, _, _, enemy = max(
            (
                group.damage_against(enemy),
                enemy.effective_power,
                enemy.initiative,
                enemy,
            )
            for enemy in enemies
        )

        if damage > 0:
            targets.append((group.initiative, group, enemy))
            available_targets.discard(enemy)

    targets = sorted(targets, reverse=True)

    # Edge case (occurs for a boost of 50): The remaining groups may have too low
    # damage to kill units in the other group, and nothing will ever happen.
    # In this case it is a draw.
    if not any(
        attacker.damage_against(defender) // defender.hp
        for _, attacker, defender in targets
    ):
        return True

    for _, attacker, defender in targets:
        defender.units = max(
            0, defender.units - attacker.damage_against(defender) // defender.hp
        )


def fight(army1, army2):
    while (
        any(g.units for g in army1.groups)
        and any(g.units for g in army2.groups)
        and not (is_draw := play_round(army1, army2))
    ):
        pass

    return None if is_draw else army1 if any(g.units for g in army1.groups) else army2


def boosted(army: Army, boost):
    for group in army.groups:
        group.damage += boost
    return army


with open("input.txt") as f:
    immune_system, infection = map(Army, f.read().split("\n\n"))


print(
    "Part 1:",
    sum(
        g.units
        for g in fight(copy.deepcopy(immune_system), copy.deepcopy(infection)).groups
    ),
)


# Perform binary search on the minimum required boost.
# First, we know boost = 0 is too low. We need an upper bound:
lower = 0
upper = 1
while (
    winner := fight(
        boosted(copy.deepcopy(immune_system), upper), copy.deepcopy(infection)
    )
) is not None and winner.name != immune_system.name:
    upper *= 2

# Now bisect [lower, upper] until lower + 1 == upper, i.e. upper is the smallest
# boost which still yields a victory for the immune system.
while lower + 1 < upper:
    c = (lower + upper) // 2
    winner = fight(boosted(copy.deepcopy(immune_system), c), copy.deepcopy(infection))
    if winner is not None and winner.name == immune_system.name:
        upper = c
    else:
        lower = c

print(
    "Part 2:",
    sum(g.units for g in fight(boosted(immune_system, upper), infection).groups),
)
