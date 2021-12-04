import numpy as np


def make_winning_mask(n):
    """(2n, n, n) mask with a (n, n) sub-masks for each winning pattern."""
    masks = np.zeros((2 * n, n, n), dtype=int)
    for i in range(n):
        masks[i, i, :] = 1
        masks[n + i, :, i] = 1
    return masks


# Parse input:
with open("input.txt") as f:
    first, *rest = f.read().strip().split("\n\n")

numbers = list(map(int, first.split(",")))
boards = np.array(
    [[[int(x) for x in line.split()] for line in board.split("\n")] for board in rest]
)

# Play the game. Keep a mask of all the selected numbers, and a mask of all the winning patterns.
selected_mask = np.zeros_like(boards)
winning_mask = make_winning_mask(boards.shape[1])
board_scores = []

for number in numbers:
    selected_mask = np.logical_or(selected_mask, boards == number)
    bingo = np.tensordot(selected_mask, winning_mask, axes=((1, 2), (1, 2))) == 5

    if (winning_board := np.where(bingo)[0]).size:
        for winner in winning_board:
            board_scores.append(number * (~selected_mask * boards)[winner].sum())

        # Stop playing on the boards that have a winning pattern by deleting them.
        selected_mask = np.delete(selected_mask, winning_board, axis=0)
        boards = np.delete(boards, winning_board, axis=0)

    if not boards.size:
        break

print("Part 1:", board_scores[0])
print("Part 2:", board_scores[-1])
