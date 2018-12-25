
def digits(num):
    return [int(d) for d in str(num)]

def iterate(recepies, elf1, elf2):
    recepies.extend(digits(recepies[elf1] + recepies[elf2]))
    return recepies, (elf1 + 1 + recepies[elf1]) % len(recepies), (elf2 + 1 + recepies[elf2]) % len(recepies)

def to_string(recepies, elf1, elf2):
    s = ''
    for i, r in enumerate(recepies):
        if i == elf1:
            s += f"({r})"
        elif i == elf2:
            s += f"[{r}]"
        else:
            s += f" {r} "
    return s



recepies = [3, 7]
elf1, elf2 = 0, 1
n_to_make = int(open('input.txt').read().strip())

while len(recepies) < n_to_make + 10:
    recepies, elf1, elf2 = iterate(recepies, elf1, elf2)

print(''.join(str(r) for r in recepies[n_to_make: n_to_make+10]))
