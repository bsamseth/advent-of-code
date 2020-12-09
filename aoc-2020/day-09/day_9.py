from toolz import sliding_window, first, last

with open("input.txt") as f:
    numbers = list(map(int, f))


def is_not_sum_of_two(nums):
    """Return True if the last number is not a sum of two other elements."""

    def rec(n, pairs):
        if not n:
            return True
        if n[0] in pairs:
            return False
        return rec(n[1:], pairs | {target - n[0]})

    target = nums[-1]
    return rec(nums[:-1], set())


def sub_sum(nums, target, low=0, high=0, current_sum=0):
    """Finds the first subarray of nums that sum to target, and returns the weakness."""

    if current_sum == target:
        return min(nums[low : high + 1]) + max(nums[low : high + 1])
    elif current_sum > target:
        return sub_sum(nums, target, low + 1, high, current_sum - nums[low])
    else:
        return sub_sum(nums, target, low, high + 1, current_sum + nums[high])


invalid = last(first(filter(is_not_sum_of_two, sliding_window(26, numbers))))
print("Part 1:", invalid)
print("Part 2:", sub_sum(numbers, target=invalid))
