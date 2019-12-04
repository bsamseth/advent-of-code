def allowed1(num):
    as_list = list(str(num))
    if not sorted(as_list) == as_list:
        return False
    for a, b in zip(as_list, as_list[1:]):
        if a == b:
            return True
    return False


def allowed2(num):
    as_list = list(str(num))
    if not sorted(as_list) == as_list:
        return False
    as_list = ["a"] + as_list + ["b"]
    for a, b, c, d in zip(as_list, as_list[1:], as_list[2:], as_list[3:]):
        if b == c and a != b and c != d:
            return True
    return False


with open("input.txt") as f:
    lower, upper = map(int, f.read().split("-"))

print("Part 1:", sum(map(allowed1, range(lower, upper + 1))))
print("Part 2:", sum(map(allowed2, range(lower, upper + 1))))
