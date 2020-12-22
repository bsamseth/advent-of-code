from collections import deque

with open("input.txt") as f:
    player1, player2 = (
        list(map(int, p.splitlines()[1:])) for p in f.read().split("\n\n")
    )


def play_combat(deck1, deck2):
    p1, p2 = map(deque, (deck1, deck2))

    while p1 and p2:
        c1, c2 = p1.popleft(), p2.popleft()
        if c1 > c2:
            p1.extend([c1, c2])
        else:
            p2.extend([c2, c1])

    return p1 or p2


def play_recursive_combat(player1_deck, player2_deck):
    memory = set()

    def recursive_combat(deck1, deck2, game):

        p1, p2 = map(deque, (deck1, deck2))

        r = 1
        while p1 and p2:
            if (sha := hash((tuple(p1), tuple(p2), game))) in memory:
                return 1, p1
            else:
                memory.add(sha)

            c1, c2 = p1.popleft(), p2.popleft()

            if len(p1) >= c1 and len(p2) >= c2:
                winner, _ = recursive_combat(list(p1)[:c1], list(p2)[:c2], game + 1)
            elif c1 > c2:
                winner = 1
            else:
                winner = 2

            if winner == 1:
                p1.extend([c1, c2])
            else:
                p2.extend([c2, c1])

            r += 1

        return 1 if p1 else 2, p1 if p1 else p2

    return recursive_combat(player1_deck, player2_deck, game=1)[1]


def score(cards):
    return sum(c * (len(cards) - i) for i, c in enumerate(cards))


print("Part 1:", score(play_combat(player1, player2)))
print("Part 2:", score(play_recursive_combat(player1, player2)))
