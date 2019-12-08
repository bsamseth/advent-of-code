CXX=g++
CXXFLAGS=-std=c++17 -Wall -Wextra -Werror

all: part1 part2

part1:  day_7_part_1.cpp intcode.o
	$(CXX) $(CXXFLAGS) $^ -o $@

intcode.o: intcode.cpp intcode.hpp
	$(CXX) $(CXXFLAGS) -c $^

clean:
	rm -f intcode.o part1 part2

.PHONY: clean
