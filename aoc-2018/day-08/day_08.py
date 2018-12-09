import numpy as np
from collections import namedtuple

Node = namedtuple("Node", "children meta")


class Tree(object):
    def __init__(self, data):
        self.data = data
        self.root = self._load_node(0)[0]

    def _load_node(self, index):
        n_children, n_meta = data[index : index + 2]
        index += 2
        node = Node([], [])
        for _ in range(n_children):
            child, index = self._load_node(index)
            node.children.append(child)
        node.meta.extend(data[index : index + n_meta])

        return node, index + n_meta

    def meta_sum(self, node=None):
        if not node:
            node = self.root
        return sum(node.meta) + sum(self.meta_sum(child) for child in node.children)

    def node_value(self, node=None):
        if not node:
            node = self.root

        if not node.children:
            return sum(node.meta)

        value = 0
        for meta in node.meta:
            if 0 <= meta - 1 < len(node.children):
                value += self.node_value(node.children[meta - 1])

        return value


data = np.loadtxt("input.txt", dtype=int, delimiter=" ")
tree = Tree(data)
print("Sum of meta data entries:", tree.meta_sum())
print("Value of root node:", tree.node_value())
