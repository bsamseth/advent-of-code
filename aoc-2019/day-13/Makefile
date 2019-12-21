CXX=g++
CXXFLAGS=-std=c++17 -Wall -Wextra -pthread -O3

day_13:  day_13.cpp intcode.hpp
	$(CXX) $(CXXFLAGS) $< -o $@

clean:
	rm -f day_13

.PHONY: clean
