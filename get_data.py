"""
Fetch data for a given day and year and output it to a text file.

By default the current day is attempted.
"""
from aocd import get_data
import argparse
import datetime

parser = argparse.ArgumentParser(description=__doc__)

parser.add_argument(
    "-y",
    "--year",
    type=int,
    default=datetime.date.today().year,
    help="Which year to fetch data for.",
)
parser.add_argument(
    "-d",
    "--day",
    type=int,
    default=datetime.date.today().day,
    help="Which day to fetch data for.",
)
parser.add_argument("output", help="Path to output file.")

args = parser.parse_args()

with open(args.output, "w") as f:
    f.write(get_data(day=args.day, year=args.year))
