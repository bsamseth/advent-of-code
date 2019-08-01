"""
Fetch input data and description for a given day and year of AoC.

By default the current day and year is attempted. The AoC session key must be provided,
by default it is assumed to be the content of a file located in ~/.config/aocd/token.

To get the session key, open the input file url when logged in to AoC and use the inspector
to look for the cookie that was sent with the request. 
"""
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
        raise Exception("Bad request: {} (verify the session key)".format(resp.status_code))

    if path:  # Input data saved as is.
        return resp.text.strip()

    # HTML rendered as markdown.
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
    help="Location of AoC session key file."
)

args = parser.parse_args()


# Check path
output = os.path.abspath(os.path.expanduser(f"aoc-{args.year}/day-{args.day}"))
args.session_key = os.path.abspath(os.path.expanduser(args.session_key))
parent_dir = os.path.dirname(output)
if not os.path.exists(parent_dir):
    raise Exception("Bad path, parent directory {} does not exist.".format(parent_dir))

if not os.path.exists(output):
    os.makedirs(output)

if not os.path.exists(args.session_key):
    raise Exception("Bad session key path: {}".format(args.session_key))

with open(args.session_key, 'r') as f:
    SESSION_KEY = f.read().strip()

# Write data file.
with open(os.path.join(output, 'input.txt'), "w") as f:
    f.write(get_content(args.year, args.day, "input", session_key=SESSION_KEY))


# Write description.
with open(os.path.join(output, 'README.md'), 'w') as f:
    f.write(get_content(args.year, args.day, session_key=SESSION_KEY))
