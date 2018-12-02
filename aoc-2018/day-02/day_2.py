from collections import Counter

with open('input1.txt', 'r') as f:
    ids = f.read().split('\n')

double_count = 0
tripple_count = 0
for i in ids:
    counts = Counter(i)
    double_count += any(count == 2 for count in counts.values())
    tripple_count += any(count == 3 for count in counts.values())

print('Checksum:', double_count * tripple_count)

