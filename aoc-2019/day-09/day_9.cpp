#include "intcode.hpp"

#include <algorithm>
#include <array>
#include <iostream>
#include <vector>
#include <boost/multiprecision/cpp_int.hpp>

using boost::multiprecision::cpp_int;

int main() {


    auto inputs = std::make_shared<IOQueue<cpp_int>>();
    auto outputs = std::make_shared<IOQueue<cpp_int>>();
    {
        inputs->push(1);
        Process p{read_program("input.txt"), inputs, outputs};
        p.join();
        std::cout << "Part 1: " << outputs->get_data().back() << std::endl;
    }
    {
        inputs->push(2);
        Process p{read_program("input.txt"), inputs, outputs};
        p.join();
        std::cout << "Part 2: " << outputs->get_data().back() << std::endl;
    }
    return 0;
}
