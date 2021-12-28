import operator
from abc import ABC, abstractmethod
from typing import Iterator, List, Literal, Optional, cast

from aocd import data, submit
from toolz import take
from toolz.curried import reduce

Bit = Literal["0", "1"]


class Packet(ABC):
    def __init__(self, version, type_id) -> None:
        self.version, self.type_id = version, type_id

    @property
    @abstractmethod
    def value(self) -> int:
        pass


class LiteralPacket(Packet):
    def __init__(self, *args, value: int) -> None:
        super().__init__(*args)
        self._value = value

    @property
    def value(self) -> int:
        return self._value


class OperatorPacket(Packet):
    length_type: Bit
    sub_packets: List[Packet]

    def __init__(self, *args, length_type: Bit, sub_packets: List[Packet]) -> None:
        super().__init__(*args)
        self.length_type, self.sub_packets = length_type, sub_packets

    @property
    def value(self) -> int:
        sub_values = [p.value for p in self.sub_packets]
        if self.type_id == 0:
            return sum(sub_values)
        elif self.type_id == 1:
            return reduce(operator.mul, sub_values)
        elif self.type_id == 2:
            return min(sub_values)
        elif self.type_id == 3:
            return max(sub_values)
        elif self.type_id == 5:
            return int(sub_values[0] > sub_values[1])
        elif self.type_id == 6:
            return int(sub_values[0] < sub_values[1])
        elif self.type_id == 7:
            return int(sub_values[0] == sub_values[1])
        else:
            raise NotImplementedError(f"Unknown operator type {self.type_id}")


def parse_packet(bit_stream: Iterator[Bit]) -> Optional[Packet]:
    version = int("".join(take(3, bit_stream)), base=2)
    type_id = int("".join(take(3, bit_stream)), base=2)

    if type_id == 4:
        value = []
        while True:
            last_group = next(bit_stream) == "0"
            value += take(4, bit_stream)
            if last_group:
                break
        return LiteralPacket(version, type_id, value=int("".join(value), base=2))

    else:
        length_type = next(bit_stream)
        n = int("".join(take(15 if length_type == "0" else 11, bit_stream)), base=2)
        if length_type == "0":
            sub_packets = list(parse_packets(take(n, bit_stream)))
        else:
            sub_packets = list(take(n, parse_packets(bit_stream)))
        return OperatorPacket(
            version, type_id, length_type=length_type, sub_packets=sub_packets
        )


def parse_packets(bit_stream: Iterator[Bit]) -> Iterator[Packet]:
    try:
        while (packet := parse_packet(bit_stream)) is not None:
            yield packet
    except Exception:
        pass


def hex_to_bits(hex_string: str) -> Iterator[Bit]:
    for char in hex_string:
        for bit in format(int(char, 16), "04b"):
            yield cast(Bit, bit)


def version_sum(packets: List[Packet]) -> int:
    s = 0
    for packet in packets:
        if isinstance(packet, OperatorPacket):
            s += version_sum(packet.sub_packets)
        s += packet.version
    return s


packet = list(parse_packets(hex_to_bits(data)))
submit(version_sum(packet), part=1)
submit(packet[0].value, part=2)
