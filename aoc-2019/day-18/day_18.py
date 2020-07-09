import heapq
from functools import lru_cache
from time import time

import networkx as nx
from networkx.exception import NetworkXNoPath


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
        robot_count = 0
        self.graph = nx.Graph()
        for i, row in enumerate(self.grid):
            for j, c in enumerate(row):
                if c == "#":
                    continue
                elif c == ".":
                    c = f".{dot_count}"
                    self.grid[i][j] = c
                    dot_count += 1
                elif c == "@":
                    c = f"@{robot_count}"
                    self.grid[i][j] = c
                    robot_count += 1

                if self.grid[i - 1][j] != "#":
                    self.graph.add_edge(c, self.grid[i - 1][j], weight=1)
                if self.grid[i][j - 1] != "#":
                    self.graph.add_edge(c, self.grid[i][j - 1], weight=1)

        self.robots = tuple(f"@{i}" for i in range(robot_count))
        self.all_keys = [c for row in self.grid for c in row if c.islower()]
        self.all_keys_set = keys_from_list(self.all_keys)

        # Prune graph: Remove all empty nodes ('.') that can be shortcutted.
        updated = True
        while updated:
            updated = False
            for a in self.graph.nodes():
                if "." in a:
                    neighbors = list(self.graph.neighbors(a))
                    if len(neighbors) == 1:
                        self.graph.remove_node(a)
                        updated = True
                        break
                    if len(neighbors) == 2:
                        neighbor, other_neighbor = neighbors
                        self.graph.add_edge(
                            neighbor,
                            other_neighbor,
                            weight=self.graph[neighbor][a]["weight"]
                            + self.graph[a][other_neighbor]["weight"],
                        )
                        self.graph.remove_node(a)
                        updated = True
                        break

    @lru_cache(maxsize=None)
    def path_from_to(self, f, t):
        if f > t:
            return self.path_from_to(t, f)

        try:
            path = nx.shortest_path(self.graph, f, t, weight="weight")
        except NetworkXNoPath:
            return False, None, None
        return (
            True,
            sum(
                self.graph[path[i]][path[i + 1]]["weight"] for i in range(len(path) - 1)
            ),
            keys_from_list([node.lower() for node in path if node.isupper()]),
        )

    def solve(self):
        distances = {(self.robots, 0): 0}
        queue = [(0, self.robots, 0)]

        while queue:
            dist, robots, cur_keys = heapq.heappop(queue)

            if cur_keys == self.all_keys_set:
                return dist

            for i, robot in enumerate(robots):
                for new_key in self.all_keys:
                    if not keys_contain(cur_keys, new_key):
                        path_exists, length, doors_blocking = self.path_from_to(
                            robot, new_key
                        )
                        if path_exists and (
                            cur_keys & doors_blocking == doors_blocking
                        ):
                            new_dist = dist + length
                            new_keys = add_key_to(cur_keys, new_key)
                            new_robots = robots[:i] + (new_key,) + robots[i + 1 :]
                            if (
                                distances.get((new_robots, new_keys), float("inf"))
                                > new_dist
                            ):
                                distances[(new_robots, new_keys)] = new_dist
                                heapq.heappush(
                                    queue, (new_dist, new_robots, new_keys,),
                                )


t0 = time()
grid = Grid("input.txt")
print(f"Part 1: {grid.solve()} (solved in {round(time() - t0, 2)} seconds)")

t0 = time()
grid = Grid("input-part2.txt")
print(f"Part 2: {grid.solve()} (solved in {round(time() - t0, 2)} seconds)")
# nx.draw_planar(grid.graph, with_labels=True)
# import matplotlib.pyplot as plt
#
# plt.show()
