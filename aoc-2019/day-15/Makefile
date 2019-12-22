CXX=g++
CXXFLAGS=-std=c++2a -Wall -Wextra -pthread -O3

day_15:  day_15.cpp intcode.hpp
	$(CXX) $(CXXFLAGS) $< -o $@

clean:
	rm -f day_15

.PHONY: clean
