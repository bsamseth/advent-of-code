cmake_minimum_required(VERSION 3.10)
project(Day9 LANGUAGES CXX)

add_executable(day_9 day_9.cpp intcode.hpp)
target_link_libraries(day_9 pthread)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra")

set_target_properties(
    day_9
      PROPERTIES
        CXX_STANDARD 17
        CXX_STANDARD_REQUIRED YES
        CXX_EXTENSIONS NO
)
