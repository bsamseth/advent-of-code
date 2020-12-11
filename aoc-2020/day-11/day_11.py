with open("input.txt") as f:
    initial_seats = f.read().strip().split("\n")


def adjacents(seats, i, j, trace):
    return sum(
        (di or dj) and see_seat(seats, i, j, di, dj, trace)
        for di in (-1, 0, 1)
        for dj in (-1, 0, 1)
    )


def see_seat(seats, i, j, di, dj, trace):
    if not (0 <= (i := i + di) < len(seats) and 0 <= (j := j + dj) < len(seats[i])):
        return False
    seat = seats[i][j]
    return (
        see_seat(seats, i, j, di, dj, trace) if trace and seat == "." else seat == "#"
    )


def new_seat_state(seats, i, j, trace):
    if seats[i][j] == ".":
        return "."

    adjacent_count = adjacents(seats, i, j, trace)
    return (
        "#"
        if seats[i][j] == "L" and adjacent_count == 0
        else "L"
        if seats[i][j] == "#" and adjacent_count >= (4 + int(trace))
        else seats[i][j]
    )


def new_state(seats, trace):
    return [
        [new_seat_state(seats, i, j, trace) for j in range(len(seats[i]))]
        for i in range(len(seats))
    ]


def count_seats_in_final_state(state, trace):
    return (
        sum(seat == "#" for row in state for seat in row)
        if (new := new_state(state, trace)) == state
        else count_seats_in_final_state(new, trace)
    )


print("Part 1:", count_seats_in_final_state(initial_seats, trace=False))
print("Part 2:", count_seats_in_final_state(initial_seats, trace=True))
