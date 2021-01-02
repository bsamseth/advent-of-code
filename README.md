# Advent of Code
Working repo for [Advent of Code](https://adventofcode.com).

Typically solved in Python, a good portion of earier years in C++ and a pinch of Haskell.

## Python dependencies

Use [poetry](https://python-poetry.org) to install dependencies from the [pyproject.toml](pyproject.toml). Written for Python 3.8 with no concern for backwards compatibility.
```bash
> poetry install
> poetry run python <path to solution>
```

## Haskell

Some solutions are written in Haskell. To run these in a way that ensures any
potential extra packages are available, use [stack](https://docs.haskellstack.org/en/stable/README/).
Once installed, the solutions can be run as stack scripts:

```
> cd aoc-20XX/day-XX
> stack day_XX.hs
```


## `init_day.py`

This script will fetch the description and input data for a given day and year. If run without any options, it tries to fetch the current day.

``` bash
> python init_day.py output-folder-path --day 25 --year 2018
```

Your AoC session key must be provided, and by default it is assumed to be the content of a file located in `~/.config/aocd/token`.
To get the session key, open the input file url when logged in to AoC and use the inspector to look for the cookie that was sent with the request.
