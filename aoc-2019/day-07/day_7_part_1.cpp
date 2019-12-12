#include "intcode.hpp"

#include <algorithm>
#include <array>
#include <iostream>
#include <vector>

/*
 * Run the program on an amplifier with a given phase and input, returning the output.
 *
 * Note that the program is taken by value, so that any modifications
 * do not affect the original program.
 */
inline int run_amplifier(const std::vector<int>& program, int phase, int io)
{
    auto inputs = std::make_shared<IOQueue<int>>();
    auto outputs = std::make_shared<IOQueue<int>>();
    inputs->push(phase);
    inputs->push(io);

    Process p {program, inputs, outputs};
    p.join();
    return outputs->get_data().back();
}

int main()
{
    auto program = read_program("input.txt");

    int best = 0;
    std::array<int, 5> phases = {0, 1, 2, 3, 4};
    do
    {
        int io = 0;
        for (int p : phases)
            io = run_amplifier(program, p, io);
        best = std::max(best, io);
    } while (std::next_permutation(phases.begin(), phases.end()));

    std::cout << "Part 1: " << best << std::endl;
}
