from toolz import reduce, last

with open("input.txt") as f:
    timestamp = int(f.readline())
    busses = [
        (i, int(bus))
        for i, bus in enumerate(f.readline().strip().split(","))
        if bus != "x"
    ]

print(
    "Part 1:",
    last(min(((delay := bus - (timestamp % bus)), delay * bus) for _, bus in busses)),
)

# Problem to solve: Find an integer x such that
#
#    x + offset_i = 0 (mod bus_id_i), for all i
# => x = -offset_i (mod bus_id_i) = bus_id_i - offset_i (mod bus_id_i)
#
# Solution given by Chinese Remainder Theorem because all the bus IDs are prime.
# Let r_i = offset_i and m_i = bus_id_i, and N = m_1 m_2 ... m_n
#
#    x = sum_i  (m_i-r_i) * N / m_i * z_i
#
# where z_i is the inverse of y_i mod m_i (that is, y_i z_i = 1 (mod m_i) ).
N = reduce(lambda a, b: a * b, map(last, busses))  # Product of all IDs (or moduli).
print("Part 2:", sum((m - r) * N // m * pow(N // m, -1, m) for r, m in busses) % N)
