from collections import Counter

with open("input1.txt", "r") as f:
    ids = f.read().strip().split("\n")

# Part 1
double_count = 0
tripple_count = 0
for i in ids:
    counts = Counter(i)
    double_count += any(count == 2 for count in counts.values())
    tripple_count += any(count == 3 for count in counts.values())

print("Checksum:", double_count * tripple_count)

# Part 2.
# The naive search (for each id, check all others for a match) is O(N^2).
# Better is O(M * N) = O(N), with M being the (max) length of the ids:

# If not all ids are the same length, use the maximum. Not an issue here.
assert all(len(i) == len(ids[0]) for i in ids)

# Remove the j-th char from each id and check for any duplicates.
done = False
for j in range(len(ids[0])):
    seen = set()
    for i in (id_i[:j] + id_i[j + 1 :] for id_i in ids):
        if i in seen:
            done = True
            break
        seen.add(i)

    if done:
        break

print("Common chracters:", i)
