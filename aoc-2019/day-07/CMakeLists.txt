cmake_minimum_required(VERSION 3.10)

add_executable(part1 intcode.cpp intcode.hpp day_7_part_1.cpp)
add_executable(part2 intcode.cpp intcode.hpp day_7_part_2.cpp)
set(CMAKE_CXX_FLAGS "-std=c++17 -Wall -Wextra -pthread")