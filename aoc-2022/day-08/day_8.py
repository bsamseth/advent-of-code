from aocd import data, submit


def scan(grid: list[list[int]], i, j, di, dj):
    count = 0
    height = grid[i][j]
    while 0 <= (i := i + di) < len(grid) and 0 <= (j := j+dj) < len(grid[0]):
        count += 1
        if grid[i][j] >= height:
            break
    return count


grid = [[int(h) for h in line] for line in data.splitlines()]
visible_count = 0
max_scenic_score = 0
for i, line in enumerate(grid):
    for j, height in enumerate(line):
        up, down, left, right = (
            scan(grid, i, j, -1, 0),
            scan(grid, i, j, 1, 0),
            scan(grid, i, j, 0, -1),
            scan(grid, i, j, 0, 1),
        )
        visible_count += (
            up == i
            or down == len(grid) - i - 1
            or left == j
            or right == len(line) - j - 1
        )
        max_scenic_score = max(max_scenic_score, up * down * left * right)


submit(part=1, answer=visible_count)
submit(part=2, answer=max_scenic_score)
