"""
This could be solved in a couple of lines with networkx, but all solutions are very slow (~1 sec).
A custom, pure Python solution is /much/ faster in this case.
"""

from collections import defaultdict


def checksum(graph, root, so_far=0):
    if root not in graph:
        return so_far

    return so_far + sum(
        checksum(graph, child, so_far=so_far + 1) for child in graph[root]
    )


def orbital_transfers_between(graph, node_a, node_b, root="COM"):
    """
    Solved by finding the common ancestor, R, of a and b, then counting
    the number of step away from R both a and b are.
    """

    def find(a, cur_node, cur_path):
        """Return the path to a, not including a it self."""
        if a == cur_node:
            return cur_path
        for child in graph[cur_node]:
            p = find(a, child, cur_path + [cur_node])
            if p:
                return p

    path_to_a = find(node_a, root, [])
    path_to_b = find(node_b, root, [])

    # Get the first common ancestor of both paths.
    ancestor = [n for n, m in zip(path_to_a, path_to_b) if n == m][-1]

    # Sum the length of the remainder of the paths to a and b, counting from the common ancestor.
    return (
        len(path_to_a)
        + len(path_to_b)
        - path_to_a.index(ancestor)
        - path_to_b.index(ancestor)
        - 2
    )


# Network represented by each non-leaf node as a key, with children as its value.
#  For COM)B, B)C, B)D we get the following representation:
#      {'COM': ['B'], 'B': ['C, D']}
nodes = defaultdict(list)
with open("input.txt") as f:
    for line in f:
        base, satellite = line.strip().split(")")
        nodes[base].append(satellite)

print("Part 1:", checksum(nodes, "COM"))
print("Part 2:", orbital_transfers_between(nodes, "YOU", "SAN"))
