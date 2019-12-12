cmake_minimum_required(VERSION 3.10)

add_executable(part1 intcode.cpp intcode.hpp day_7_part_1.cpp)
set(CMAKE_CXX_FLAGS "-std=c++17 -Wall -Wextra -pthread")