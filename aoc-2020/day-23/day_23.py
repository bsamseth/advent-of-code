import time

t0 = time.time()


class Node:
    def __init__(self, value, next=None):
        self.value, self.next = value, next

    def __repr__(self):
        return f"Node({self.value})"


class CircularList:
    def __init__(self, iterable=()):
        self.head = self.tail = None
        self.value_to_node = {v: self._append(v) for v in iterable}

    def __len__(self):
        return len(self.value_to_node)

    def __iter__(self):
        if self.head is None:
            return iter(())
        curr = self.head
        while True:
            yield curr.value
            curr = curr.next
            if curr == self.head:
                break

    def __getitem__(self, v):
        return self.value_to_node[v]

    def _append(self, v):
        n = Node(v, next=self.head)
        if self.tail is None:
            self.head = self.tail = n
            n.next = n
        else:
            self.tail.next = n
            self.tail = n
        return n

    def rotate(self):
        self.head, self.tail = self.head.next, self.head


def move(order: CircularList) -> None:
    curr = order.head
    a = order.head.next
    b = order.head.next.next
    c = order.head.next.next.next

    d = curr.value
    while (d := d - 1) in (a.value, b.value, c.value) or d == 0:
        if d == 0:
            d = len(order) + 1

    curr.next = c.next
    d = order[d]
    c.next = d.next
    d.next = a
    order.rotate()


with open("input.txt") as f:
    starting_order = list(map(int, list(f.read().strip())))

ll = CircularList(starting_order)
for _ in range(100):
    move(ll)
while ll.head.value != 1:
    ll.rotate()
print("Part 1:", "".join(map(str, ll))[1:])


ll = CircularList(starting_order + list(range(len(starting_order) + 1, 1_000_000 + 1)))
for _ in range(10_000_000):
    move(ll)
print("Part 2:", ll[1].next.value * ll[1].next.next.value)
print(f"Total time: {round(time.time()-t0, 2)} seconds")
