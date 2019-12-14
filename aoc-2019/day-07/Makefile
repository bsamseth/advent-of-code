CXX=g++
CXXFLAGS=-std=c++17 -Wall -Wextra -pthread

day_7:  day_7.cpp intcode.o
	$(CXX) $(CXXFLAGS) $^ -o $@

intcode.o: intcode.cpp intcode.hpp
	$(CXX) $(CXXFLAGS) -c $<

clean:
	rm -f intcode.o day_7

.PHONY: clean
