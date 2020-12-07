# Advent of Code
Working repo for [Advent of Code](https://adventofcode.com).

Typically solved in Python, a good portion of earier years in C++ and a pinch of Haskell.

## Dependencies

Use [poetry](https://python-poetry.org) to install dependencies from the [pyproject.toml](pyproject.toml). Written for Python 3.8 with no concern for backwards compatibility.
```bash
> poetry install
> poetry run python <path to solution>
```

## `init_day.py`

This script will fetch the description and input data for a given day and year. If run without any options, it tries to fetch the current day.

``` bash
> python init_day.py output-folder-path --day 25 --year 2018
```
