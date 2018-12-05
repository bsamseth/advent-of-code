"""
Fetch data and description for a given day and year and output it given folder.

By default the current day is attempted. The AoC session key must be provided,
by default it is assumed to be located in ~/.config/aocd/token.
"""
# from aocd import get_data
import argparse
import datetime
import os
import sys
import requests
from bs4 import BeautifulSoup
from tomd import Tomd


def get_content(year, day, path='', session_key=None):
    url = "https://adventofcode.com/{}/day/{}".format(year, day)
    if path:
        url += '/' + path

    resp = requests.get(url, headers={'cookie':'session={};'.format(session_key)})

    if not resp.ok:
        raise Exception("Bad request: {}".format(resp.status_code))

    if path:
        return resp.text.strip()

    soup = BeautifulSoup(resp.text, 'html.parser')
    return Tomd(str(soup.main)).markdown



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
parser.add_argument(
    "--session-key",
    default="~/.config/aocd/token",
    help="Location of AoC session key."
)
parser.add_argument("output", help="Path to output file.")

args = parser.parse_args()


# Check path
args.output = os.path.abspath(os.path.expanduser(args.output))
args.session_key = os.path.abspath(os.path.expanduser(args.session_key))
parent_dir = os.path.dirname(args.output)
if not os.path.exists(parent_dir):
    raise Exception("Bad path, parent directory {} does not exist.".format(parent_dir))

if not os.path.exists(args.output):
    os.makedirs(args.output)

if not os.path.exists(args.session_key):
    raise Exception("Bad session key path: {}".format(args.session_key))

with open(args.session_key, 'r') as f:
    SESSION_KEY = f.read().strip()

# Write data file.
with open(os.path.join(args.output, 'input.txt'), "w") as f:
    f.write(get_content(args.year, args.day, "input", session_key=SESSION_KEY))


# Write description.
with open(os.path.join(args.output, 'README.md'), 'w') as f:
    f.write(get_content(args.year, args.day, session_key=SESSION_KEY))
