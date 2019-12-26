#include "intcode.hpp"

using Data = int;  // It appears this didn't need big integers. (if needed:
                   // boost::multiprecision::cpp_int);

int main()
{
    Process<Data> process {"input.txt"};

    process.join();
}
