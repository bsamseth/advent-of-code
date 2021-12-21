import networkx as nx


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


with open("input.txt") as f:
    graph = nx.Graph([line.strip().split("-") for line in f])


print("Part 1:", traverse(graph, [], "start"))
print("Part 2:", traverse(graph, [], "start", allow_twice_visit=True))
