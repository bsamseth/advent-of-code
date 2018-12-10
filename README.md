# Advent of Code
Working repo for [Advent of Code](https://adventofcode.com).

## Dependencies

Use [pipenv](https://pipenv.readthedocs.io/en/latest/) to install dependecies from the [Pipfile](Pipfile). Written for Python 3.7, with no concern for backwards compatibility.
```bash
> pipenv install
> pipenv shell
```

## `init_day.py`

This script will fetch the description and input data for a given day and year. If run without any options, it tries to fetch the current day.

``` bash
> python init_day.py output-folder-path --day 25 --year 2018
```
