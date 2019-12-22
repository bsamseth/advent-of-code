#include "intcode.hpp"

#include <iostream>
#include <memory>
#include <map>
#include <algorithm>
#include <boost/multiprecision/cpp_int.hpp>

using Data = int; //boost::multiprecision::cpp_int;
using Point = std::pair<Data, Data>;

enum class Status {
    WALL = 0, MOVED, FOUND
};


int main() {
    auto assembly = read_program<Data>("input-hacked.txt");
}
