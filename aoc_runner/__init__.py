import io
import subprocess
import sys
from pathlib import Path


def plugin(year, day, **kwargs):
    script = Path(__file__).parent.parent / f"aoc-{year}/day-{day:02d}/day_{day}.py"

    if not script.exists():
        return None, None

    old_stdout = sys.stdout
    sys.stdout = io.StringIO()
    try:
        p = subprocess.run(
            ["poetry", "run", "python", f"day_{day}.py"],
            capture_output=True,
            cwd=script.parent,
        )
    finally:
        sys.stdout = old_stdout

    part_1, *part_2 = [
        int(x.strip().split(" ")[-1]) for x in p.stdout.decode().splitlines()
    ]
    return part_1, part_2[0] if part_2 else None
