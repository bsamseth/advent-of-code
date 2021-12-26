import re
import subprocess
from pathlib import Path


def _as_int(x):
    try:
        return int(x)
    except ValueError:
        return x


def plugin(year, day, **kwargs):
    script = (
        Path(__file__).parent.parent / f"aoc-{year}/day-{day:02d}/day_{day}"
    ).absolute()

    if script.exists():
        # A binary already exists, so we can just run it and assume it is fast:
        command = [script]
    elif script.with_suffix(".cpp").exists():
        command = (
            f"g++ -std=c++17 -O3 -o {script} {script.with_suffix('.cpp')} && {script}"
        )
    elif script.with_suffix(".hs").exists():
        command = f"stack {script.with_suffix('.hs')}"
    elif script.with_suffix(".py").exists():
        command = f"python {script.with_suffix('.py')}"
    else:
        raise FileNotFoundError("No code for this day.")

    p = subprocess.run(command, capture_output=True, cwd=script.parent, shell=True)

    part_1, *part_2 = re.findall(
        r"^[pP]art.*:\s*(.+)$", p.stdout.decode(), flags=re.MULTILINE
    )
    return _as_int(part_1), _as_int(part_2[0]) if part_2 else None
