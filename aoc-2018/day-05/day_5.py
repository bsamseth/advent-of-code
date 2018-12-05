with open("input.txt", "r") as f:
    data = f.read().strip()

i = 0
while i < len(data) - 1:

    # Lowercase - uppercase differs by 32 in ASCII
    if abs(ord(data[i]) - ord(data[i + 1])) == 32:
        data = data[:i] + data[i + 2 :]
        i = max(0, i - 1)  # Backtrack one step to account for new possible match.

    else:
        i += 1

    print("Progress: {:05d}".format(i), end="\r")

print("Polymer contains {} units after fully reacting.".format(len(data)))
