cmake_minimum_required(VERSION 3.10)
project(Day15 LANGUAGES CXX)
add_executable(day_15 day_15.cpp intcode.hpp)
target_link_libraries(day_15 pthread)

set_target_properties(
    day_15
      PROPERTIES
        CXX_STANDARD 17
        CXX_STANDARD_REQUIRED YES
        CXX_EXTENSIONS NO
)
