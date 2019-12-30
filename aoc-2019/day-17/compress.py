from string import ascii_uppercase


def remove_sublist(sub, lst, replace_by=""):
    sub_joined = "-".join(sub)
    lst_joined = "-".join(lst)
    lst_joined = (
        lst_joined.replace(sub_joined, replace_by).replace("--", "-").strip("-")
    )
    return lst_joined.split("-")


path_string = (
    "L,4,L,4,L,10,R,4,R,4,L,4,L,4,R,8,R,10,L,4,L,4,L,10,R,4,R,4,L,10,R,10,L,4,L,4,L,10,R,4,R,4,L,10,R,10,R,4,L,4,"
    "L,4,R,8,R,10,R,4,L,10,R,10,R,4,L,10,R,10,R,4,L,4,L,4,R,8,R,10"
)

path = [
    ",".join([a, b])
    for a, b in zip(path_string.split(",")[::2], path_string.split(",")[1::2])
]

# Mapping all turn-step pairs into unique characters, using letters Z, Y, X, W, V...
step_to_char = dict(zip(sorted(set(path)), ascii_uppercase[::-1]))
char_to_step = {v: k for k, v in step_to_char.items()}

path_mapped = [step_to_char[step] for step in path]

A = ["Y", "Y", "Z", "W"]
B = ["W", "Z", "X"]
C = ["W", "Y", "Y", "V", "X"]

print(
    ",".join(
        remove_sublist(
            C, remove_sublist(B, remove_sublist(A, path_mapped, "A"), "B"), "C"
        )
    )
)
print("A:", ",".join(char_to_step[char] for char in A))
print("B:", ",".join(char_to_step[char] for char in B))
print("C:", ",".join(char_to_step[char] for char in C))
