cmake_minimum_required(VERSION 3.10)
add_executable(day_7 intcode.cpp intcode.hpp day_7.cpp)
set(CMAKE_CXX_FLAGS "-std=c++17 -Wall -Wextra -pthread")