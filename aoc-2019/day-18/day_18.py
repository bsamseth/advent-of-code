import heapq
from functools import lru_cache
from time import time

import networkx as nx


def add_key_to(keys, key):
    return keys | (1 << (ord(key.lower()) - ord("a")))


def keys_contain(keys, key):
    return keys & (1 << (ord(key.lower()) - ord("a")))


def keys_from_list(char_keys):
    keys = 0
    for key in char_keys:
        keys = add_key_to(keys, key)
    return keys


class Grid:
    def __init__(self, filename):
        with open(filename) as f:
            self.grid = [list(line.strip()) for line in f]

        dot_count = 0
        self.graph = nx.Graph()
        for i, row in enumerate(self.grid):
            for j, c in enumerate(row):
                if c == "#":
                    continue

                if c == "@":
                    self.pos = i, j, "@"

                if c == "." and (
                    (
                        row[j - 1] != "#"
                        and row[j + 1] != "#"
                        and self.grid[i - 1][j] == "#"
                        and self.grid[i + 1][j] == "#"
                    )
                    or (
                        row[j - 1] == "#"
                        and row[j + 1] == "#"
                        and self.grid[i - 1][j] != "#"
                        and self.grid[i + 1][j] != "#"
                    )
                ):
                    # Node is not interesting, just a hallway.
                    continue

                if c == ".":
                    c = f".{dot_count}"
                    self.grid[i][j] = c
                    dot_count += 1

                self.graph.add_node(c)

                for k in reversed(range(j)):
                    if self.grid[i][k] in self.graph:
                        self.graph.add_edge(self.grid[i][k], c, weight=j - k)
                        break
                    elif self.grid[i][k] == "#":
                        break
                for k in reversed(range(i)):
                    if self.grid[k][j] in self.graph:
                        self.graph.add_edge(self.grid[k][j], c, weight=i - k)
                        break
                    elif self.grid[k][j] == "#":
                        break

        self.all_keys = [c for row in self.grid for c in row if c.islower()]
        self.all_keys_set = keys_from_list(self.all_keys)

    @lru_cache(maxsize=None)
    def path_from_to(self, f, t):
        if f > t:
            return self.path_from_to(t, f)

        path = nx.shortest_path(self.graph, f, t, weight="weight")
        return (
            sum(
                self.graph[path[i]][path[i + 1]]["weight"] for i in range(len(path) - 1)
            ),
            keys_from_list([node.lower() for node in path if node.isupper()]),
        )

    def solve_my(self):
        distances = {("@", 0): 0}
        queue = [(0, "@", 0)]

        while queue:
            dist, cur_key, cur_keys = heapq.heappop(queue)

            if cur_keys == self.all_keys_set:
                return dist

            for new_key in self.all_keys:
                if not keys_contain(cur_keys, new_key):
                    length, doors_blocking = self.path_from_to(cur_key, new_key)
                    if cur_keys & doors_blocking == doors_blocking:
                        new_dist = dist + length
                        new_keys = add_key_to(cur_keys, new_key)
                        if distances.get((new_key, new_keys), float("inf")) > new_dist:
                            distances[(new_key, new_keys)] = new_dist
                            heapq.heappush(queue, (new_dist, new_key, new_keys))


t0 = time()
grid = Grid("input.txt")
print(f"Part 1: {grid.solve_my()} (solved in {round(time() - t0, 2)} seconds)")
