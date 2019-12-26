cmake_minimum_required(VERSION 3.10)
project(Day17 LANGUAGES CXX)
add_executable(day_17 day_17.cpp intcode.hpp)
target_link_libraries(day_17 pthread)

set_target_properties(
    day_17
      PROPERTIES
        CXX_STANDARD 17
        CXX_STANDARD_REQUIRED YES
        CXX_EXTENSIONS NO
)
