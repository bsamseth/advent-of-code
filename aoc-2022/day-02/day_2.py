from aocd import data, submit


def score_1(them: str, us: str) -> int:
    outcome = {
        "A": {"X": 1, "Y": 2, "Z": 0},
        "B": {"X": 0, "Y": 1, "Z": 2},
        "C": {"X": 2, "Y": 0, "Z": 1},
    }
    return 3 * outcome[them][us] + ord(us) - ord("W")


def score_2(them: str, result: str) -> int:
    playbook = {
        "X": {"A": "Z", "B": "X", "C": "Y"},
        "Y": {"A": "X", "B": "Y", "C": "Z"},
        "Z": {"A": "Y", "B": "Z", "C": "X"},
    }
    us = playbook[result][them]
    return score_1(them, us)


rounds = [round.split() for round in data.strip().splitlines()]
submit(part=1, answer=sum(score_1(*round) for round in rounds))
submit(part=2, answer=sum(score_2(*round) for round in rounds))
