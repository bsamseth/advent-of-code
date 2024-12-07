# Advent of Code
Solutions to [Advent of Code](https://adventofcode.com) puzzles.

Many days solved in Python, a good portion of earier years in C++, a pinch of Haskell, and more recently using Rust.
All days are written as standalone solutions and can be run independently.

## Python

Use [poetry](https://python-poetry.org) to install dependencies from the [pyproject.toml](pyproject.toml).
```bash
> poetry install
> cd <path to solution>
> poetry run python day_XX.py
```

## C++

Compile using any reasonably recent C++ compiler, unless otherwise specified:
```bash
> cd aoc-20XX/day-XX
> g++ -O3 -o day_XX day_XX.cpp
```

## Haskell

To run Haskell solutions in a way that ensures any potential extra packages are available, use
[stack](https://docs.haskellstack.org/en/stable/README/). Once installed, the solutions can be run as stack scripts:

```bash
> cd aoc-20XX/day-XX
> stack day_XX.hs
```

## Rust

Each rust solution is written as a binary crate, and can be compiled and run using Cargo.

```bash
> cd aoc-20XX/day-XX
> cargo run --release
```


## `initday`

This will setup for a given day and year. If run without any options, it tries to fetch the current day.

``` bash
> ./initday --day 25 --year 2018
```

Your AoC session key must be provided, and by default it is assumed to be the
content of a file located in `~/.config/aocd/token`. To get the session key,
open the input file url when logged in to AoC and use the inspector to look for
the cookie that was sent with the request.
