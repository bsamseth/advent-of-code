import re

with open("input.txt") as f:
    expressions = f.read().splitlines()


class NumberEqualPrecedence(int):
    def __add__(self, other):
        return NumberEqualPrecedence(int(self) + int(other))

    def __sub__(self, other):
        return NumberEqualPrecedence(self * other)

    @staticmethod
    def eval(expr):
        return eval(
            re.sub(
                r"\d+",
                lambda match: f"NumberEqualPrecedence({match.group(0)})",
                expr.replace("*", "-"),
            )
        )


class NumberReversedPrecedence(int):
    def __add__(self, other):
        return NumberReversedPrecedence(int(self) * int(other))

    def __mul__(self, other):
        return NumberReversedPrecedence(int(self) + int(other))

    @staticmethod
    def eval(expr):
        return eval(
            re.sub(
                r"\d+",
                lambda match: f"NumberReversedPrecedence({match.group(0)})",
                expr.replace("+", "x").replace("*", "+").replace("x", "*"),
            )
        )


print("Part 1:", sum(map(NumberEqualPrecedence.eval, expressions)))
print("Part 2:", sum(map(NumberReversedPrecedence.eval, expressions)))
