from aocd import data, submit


def marker_with_len(n: int) -> int:
    for i in range(0, len(data)-n):
        chars = data[i:i+n]
        if len(set(chars)) == len(chars):
            return i + n
        
submit(part=1, answer=marker_with_len(4))
submit(part=2, answer=marker_with_len(14))
