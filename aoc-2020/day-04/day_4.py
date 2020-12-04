import re
from typing import Optional

from pydantic import BaseModel, conint, constr, validator


class Passport(BaseModel):
    byr: conint(ge=1920, le=2002)
    iyr: conint(ge=2010, le=2020)
    eyr: conint(ge=2020, le=2030)
    hgt: constr(regex=r"^\d+(in|cm)$")
    hcl: constr(regex=r"^#[0-9a-f]{6}$")
    ecl: constr(regex=r"^(amb|blu|brn|gry|grn|hzl|oth)$")
    pid: constr(regex=r"^\d{9}$")
    cid: Optional[str]

    @validator("hgt")
    def height_size(cls, v):
        number, unit = int(v[:-2]), v[-2:]
        low, hi = (150, 193) if unit == "cm" else (59, 76)
        assert low <= number <= hi
        return v


with open("input.txt") as f:
    passports = [
        dict(re.findall(r"(\S+):(\S+)", raw)) for raw in f.read().strip().split("\n\n")
    ]

required = {f.name for f in Passport.__fields__.values() if f.required}
print("Part 1:", sum(required.issubset(p) for p in passports))

valid = 0
for p in passports:
    try:
        Passport.validate(p)
    except ValueError:
        pass
    else:
        valid += 1

print("Part 2:", valid)
