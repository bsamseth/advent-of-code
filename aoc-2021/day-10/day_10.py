pairs = {")": "(", "}": "{", "]": "[", ">": "<"}
syntax_error_points = {")": 3, "]": 57, "}": 1197, ">": 25137}
completion_points = {"(": 1, "[": 2, "{": 3, "<": 4}
error_score = 0
completion_scores = []
with open("input.txt") as f:
    for line in f:
        stack = []
        for char in line:
            if char in ("(", "[", "{", "<"):
                stack.append(char)
            elif char in (")", "]", "}", ">"):
                start = stack.pop()
                if pairs[char] != start:
                    error_score += syntax_error_points[char]
                    break
        else:
            if stack:
                completion_score = 0
                for char in reversed(stack):
                    completion_score = completion_score * 5 + completion_points[char]
                completion_scores.append(completion_score)


print("Part 1:", error_score)
print("Part 2:", sorted(completion_scores)[len(completion_scores) // 2])
