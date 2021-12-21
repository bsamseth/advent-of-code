import networkx as nx
from aocd import lines, submit


def traverse(graph, path, node, allow_twice_visit=False):
    if node == "end":
        return 1

    if node.lower() == node and node in path:
        if allow_twice_visit and node != "start":
            allow_twice_visit = False
        else:
            return 0

    path.append(node)
    n_paths = sum(
        traverse(graph, path, n, allow_twice_visit=allow_twice_visit)
        for n in graph.neighbors(node)
    )
    path.pop()
    return n_paths


graph = nx.Graph([line.strip().split("-") for line in lines])
submit(traverse(graph, [], "start"), part=1)
submit(traverse(graph, [], "start", allow_twice_visit=True), part=2)
