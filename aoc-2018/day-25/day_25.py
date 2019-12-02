import numpy as np


def add_to_cluster(point, clusters):
    for cluster in clusters:
        for p in cluster:
            if np.sum(np.abs(point - p)) <= 3:
                cluster.append(point)
                return
    clusters.append([point])


def merge_clusters(clusters):
    for i, c1 in enumerate(clusters):
        for j, c2 in enumerate(clusters[i + 1 :]):
            for p1 in c1:
                for p2 in c2:
                    if np.sum(np.abs(p1 - p2)) <= 3:
                        leading = clusters[:i]
                        middle = clusters[i + 1 : i + 1 + j]
                        trailing = clusters[i + 1 + j + 1 :]
                        joined = c1 + c2

                        return merge_clusters(leading + middle + trailing + [joined])
    return clusters


data = np.loadtxt("input.txt", dtype=int, delimiter=",")
clusters = []
for point in data:
    add_to_cluster(point, clusters)

merged = merge_clusters(clusters)
print(len(merged))
