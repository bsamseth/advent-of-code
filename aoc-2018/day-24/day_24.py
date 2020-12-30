import re


class Group:
    def __init__(self, army, desc):
        self.army = army
        m = re.search(
            r"""
            (?P<units>\d+)                                     # Unit count
            \sunits\seach\swith\s(?P<hp>\d+)\shit\spoints\s    # Hit points
            (\(
              (immune\sto\s                                    # Immunities
               (?P<immune>(\w+(,\s)?)+)(\)|;\s)
              )?
              (weak\sto\s                                      # Weaknesses
                (?P<weak>(\w+(,\s)?)+)
              )?
            \))?
            \s?with\san\sattack\sthat\sdoes
            \s(?P<damage>\d+)\s(?P<damage_type>\w+)            # Damge points and type
            \sdamage\sat\sinitiative\s(?P<initiative>\d+)      # Initiative
            """,
            desc,
            flags=re.VERBOSE,
        )
        if m is None:
            print()

        (
            self.units,
            self.hp,
            self.initiative,
            self.damage,
            self.damage_type,
            self.weak,
            self.immune,
        ) = (
            int(m.group("units")),
            int(m.group("hp")),
            int(m.group("initiative")),
            int(m.group("damage")),
            m.group("damage_type"),
            m.group("weak").split(", ") if m.group("weak") else [],
            m.group("immune").split(", ") if m.group("immune") else [],
        )

    @property
    def effective_power(self):
        return self.units * self.damage

    def damage_against(self, enemy: "Group"):
        immune = self.damage_type in enemy.immune
        weak = self.damage_type in enemy.weak
        return int(not immune) * (1 + int(weak)) * self.effective_power

    def __repr__(self):
        return f"Group({self.units=})"


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

    for _, attacker, defender in targets:
        defender.units = max(
            0, defender.units - attacker.damage_against(defender) // defender.hp
        )


with open("input.txt") as f:
    immune_system, infection = map(Army, f.read().split("\n\n"))


while any(g.units for g in immune_system.groups) and any(
    g.units for g in infection.groups
):
    play_round(immune_system, infection)
    # print(immune_system.groups, infection.groups)

print(
    "Part 1:", sum(g.units for army in (immune_system, infection) for g in army.groups)
)
