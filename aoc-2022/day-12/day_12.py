import networkx as nx
from aocd import data, submit


def height(char: str) -> int:
    if char == "S":
        char = "a"
    elif char == "E":
        char = "z"
    return ord(char) - ord("a")


def path_length(
    graph: nx.Graph, source: tuple[int, int], target: tuple[int, int]
) -> int:
    try:
        return nx.shortest_path_length(graph, source, target)
    except nx.NetworkXNoPath:
        return 2**63 - 1


grid: list[list[str]] = list(map(list, data.splitlines()))
graph = nx.DiGraph()
e = None
for i, row in enumerate(grid):
    for j, col in enumerate(row):
        if col == "E":
            e = (i, j)
        h = height(col)
        graph.add_node((i, j), height=h)
        if i > 0 and height(grid[i - 1][j]) <= h + 1:
            graph.add_edge((i, j), (i - 1, j))
        if i < len(grid) - 1 and height(grid[i + 1][j]) <= h + 1:
            graph.add_edge((i, j), (i + 1, j))
        if j > 0 and height(grid[i][j - 1]) <= h + 1:
            graph.add_edge((i, j), (i, j - 1))
        if j < len(row) - 1 and height(grid[i][j + 1]) <= h + 1:
            graph.add_edge((i, j), (i, j + 1))


assert e is not None
paths = [
    path_length(graph, (i, j), e) for (i, j) in graph.nodes if grid[i][j] in ["S", "a"]
]
submit(part=1, answer=paths[0])
submit(part=2, answer=min(paths))
