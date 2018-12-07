import re
import copy
from collections import defaultdict
import heapq


class Worker(object):
    def __init__(self):
        self.task_time = 0
        self.task = None

    @property
    def idle(self):
        return self.task_time == 0

    def recieve_task(self, task, task_time):
        self.task = task
        self.task_time = task_time

    def do_work(self):
        self.task_time = max(0, self.task_time - 1)


class Graph(object):
    def __init__(self):
        self.graph = defaultdict(set)

    def add_edge(self, step, dependency):
        self.graph[step].add(dependency)
        if dependency not in self.graph:
            self.graph[dependency] = set()

    def parallell_schedule(self, n_workers, extra_per_task=0):
        """Return order of tasks in graph and time to process them."""
        graph = copy.deepcopy(self.graph)
        workers = [Worker() for _ in range(n_workers)]

        topo_sort = []
        heap = [node for node in graph if not graph[node]]
        remaining = [node for node in graph if graph[node]]
        heapq.heapify(heap)

        time = -2
        while True:
            # Tick clock forward one step.
            time += 1
            for worker in workers:
                worker.do_work()

                # If task has been completed:
                if worker.idle and worker.task is not None:
                    # Add this task next in the sequence.
                    topo_sort.append(worker.task)

                    # Remove task from dependencies.
                    for node in remaining[:]:
                        graph[node] = graph[node].difference({worker.task})

                        # Steps with no more deps should go into the ready heap.
                        if not graph[node]:
                            heapq.heappush(heap, node)
                            remaining.remove(node)

                    worker.task = None

            # Distrubute as many available tasks as possible.
            if heap:
                for worker in workers:
                    if worker.idle:
                        task = heapq.heappop(heap)
                        task_time = ord(task) - ord("A") + 1 + extra_per_task
                        worker.recieve_task(task, task_time)

                        if not heap:  # No more tasks to distribute.
                            break

            # Done when all workers are done at the same time.
            if all(worker.idle for worker in workers):
                break

        return topo_sort, time + 1


with open("input.txt", "r") as f:
    graph = Graph()
    for line in f:
        match = re.match("Step ([A-Z]).*step ([A-Z])", line)
        if match:
            dep, step = match.groups()
            graph.add_edge(step, dep)

print("Order of steps:", "".join(graph.parallell_schedule(1)[0]))
print("Parallell time of steps:", graph.parallell_schedule(5, extra_per_task=60)[1])
