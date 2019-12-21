cmake_minimum_required(VERSION 3.10)
project(Day13 LANGUAGES CXX)
add_executable(day_13 day_13 intcode.hpp)
target_link_libraries(day_13 pthread)

set_target_properties(
    day_13
      PROPERTIES
        CXX_STANDARD 17
        CXX_STANDARD_REQUIRED YES
        CXX_EXTENSIONS NO
)
