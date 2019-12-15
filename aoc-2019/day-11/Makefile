CXX=g++
CXXFLAGS=-std=c++17 -Wall -Wextra -pthread -O3

day_11:  day_11.cpp intcode.hpp
	$(CXX) $(CXXFLAGS) $< -o $@

clean:
	rm -f day_11

.PHONY: clean
