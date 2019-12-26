CXX=g++
CXXFLAGS=-std=c++2a -Wall -Wextra -pthread -O3

day_17:  day_17.cpp intcode.hpp
	$(CXX) $(CXXFLAGS) $< -o $@

clean:
	rm -f day_17

.PHONY: clean
