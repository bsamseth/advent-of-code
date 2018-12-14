from itertools import cycle
import re
from colorama import Fore, Style

class Cart(object):
    carts_chars = ["<", ">", "^", "v"]
    direction = dict(zip(carts_chars, [(-1, 0), (1, 0), (0, -1), (0, 1)]))
    left = dict(zip(carts_chars, "v^<>"))
    straight = dict(zip(carts_chars, carts_chars))
    right = dict(zip(carts_chars, "^v><"))
    forward_slash = dict(zip(carts_chars, "v^><"))
    backward_slash = dict(zip(carts_chars, "^v<>"))
    def __init__(self, symbol, x, y):
        self.symbol, self.x, self.y = symbol, x, y
        self.turns = cycle([Cart.left, Cart.straight, Cart.right])
        self.crashed = False

    def move(self, grid):
        dx, dy = Cart.direction[self.symbol]
        self.x, self.y = self.x + dx, self.y + dy

        if grid[self.y][self.x] in Cart.carts_chars:
            print("Collision!", self.x, self.y)
            return False,  # Crash!

        if grid[self.y][self.x] == "+":
            self.symbol = next(self.turns)[self.symbol]
        elif grid[self.y][self.x] == "/":
            self.symbol = Cart.forward_slash[self.symbol]
        elif grid[self.y][self.x] == "\\":
            self.symbol = Cart.backward_slash[self.symbol]

        return True, self.x, self.y, self.symbol

    def __repr__(self):
        return f"Cart({self.symbol}, {self.x}, {self.y})"


class Grid(object):
    def __init__(self, filename):
        with open(filename, "r") as f:
            self.grid = [[char for char in line if char != "\n"] for line in f if line != "\n"]

        # Input should be formated with equal width per row, but editors might muck this up.
        assert all(len(row) == len(self.grid[0]) for row in self.grid)

        # Obtain a copy of the grid with all carts removed. Needed to restore
        # grid positions after carts move.
        self.empty_grid = [
            list(
                "".join(row)
                .replace(">", "-")
                .replace("<", "-")
                .replace("^", "|")
                .replace("v", "|")
            )
            for row in self.grid
        ]
        self.carts = [Cart(char, j, i) for i, row in enumerate(self.grid) for j, char in enumerate(row) if char in Cart.carts_chars]

    def __str__(self):
        s = "\n".join("".join(row) for row in self.grid)
        s = re.sub("[<>^v]", lambda match: f"{Fore.RED}{Style.BRIGHT}{match.group(0)}{Style.RESET_ALL}", s)
        return s


    def tick(self):
        for cart in filter(lambda c: not c.crashed, self.carts):
            self.grid[cart.y][cart.x] = self.empty_grid[cart.y][cart.x]
            success, *result = cart.move(self.grid)
            if not success:
                self.grid[cart.y][cart.x] = self.empty_grid[cart.y][cart.x]
                for c in self.carts:
                    if cart.x == c.x and cart.y == c.y:
                        c.crashed = True
            else:
                x, y, symbol = result
                self.grid[y][x] = symbol
        self.carts = list(filter(lambda c: not c.crashed, self.carts))

if __name__ == "__main__":
    grid = Grid('input.txt')
    while len(grid.carts) > 1:
        # print(grid)
        grid.tick()
    print(grid)
    print(grid.carts)
