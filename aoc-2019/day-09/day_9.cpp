#include "intcode.hpp"

#include <iostream>
#include <boost/multiprecision/cpp_int.hpp>

using boost::multiprecision::cpp_int;

int main() {

    for (int part = 1; part <= 2; ++part) {
        auto inputs = std::make_shared<IOQueue<cpp_int>>();
        auto outputs = std::make_shared<IOQueue<cpp_int>>();
        inputs->push(part);
        Process<cpp_int> p{read_program<cpp_int>("input.txt"), inputs, outputs};
        p.join();
        std::cout << "Part " << part << ": " << outputs->get_data().back() << std::endl;
    }
    return 0;
}
