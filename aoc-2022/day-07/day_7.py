from dataclasses import dataclass
from typing import Iterator, Optional, Union

from aocd import data, submit


@dataclass
class Dir:
    name: str
    contents: list[Union["Dir", "File"]]
    parent: Optional["Dir"] = None

    @property
    def path(self) -> str:
        if self.parent is None:
            return "/"
        return f"{self.parent.path}{self.name}/"

    @property
    def size(self) -> int:
        return sum(child.size for child in self.contents)


@dataclass
class File:
    name: str
    size: int


def parse(data: str) -> Dir:
    lines = iter(data.splitlines()[1:])
    dirs = {"/": Dir(name="", contents=[])}
    cwd = dirs["/"]

    for line in lines:
        if line.startswith("$ ls"):
            continue
        elif line == "$ cd ..":
            assert cwd.parent is not None
            cwd = cwd.parent
        elif line.startswith("$ cd "):
            dir_name = line[5:]
            path = f"{cwd.path}{dir_name}/"
            cwd = dirs[path]
        elif line.startswith("dir "):
            dir_name = line[4:]
            path = f"{cwd.path}{dir_name}/"
            if path not in dirs:
                dirs[path] = Dir(name=dir_name, contents=[], parent=cwd)
                cwd.contents.append(dirs[path])
        else:
            size, name = line.split()
            file = File(name, int(size))
            cwd.contents.append(file)
    return dirs["/"]


def dirs_with_size_at_most(dir_: Dir, n: int) -> Iterator[Dir]:
    if dir_.size <= n:
        yield dir_
    for child in dir_.contents:
        if isinstance(child, Dir):
            yield from dirs_with_size_at_most(child, n)


def sizes(dir_: Dir) -> Iterator[int]:
    yield dir_.size
    for child in dir_.contents:
        if isinstance(child, Dir):
            yield from sizes(child)


root = parse(data)
submit(part=1, answer=sum(dir.size for dir in dirs_with_size_at_most(root, 100000)))
submit(
    part=2,
    answer=next(
        size
        for size in sorted(sizes(root))
        if size >= 30000000 - (70000000 - root.size)
    ),
)
