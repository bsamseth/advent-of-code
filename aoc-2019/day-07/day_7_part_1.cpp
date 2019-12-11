#include <iostream>
#include <vector>
#include <array>
#include <algorithm>

#include "intcode.hpp"

/*
 * Run the program on an amplifier with a given phase and input, returning the output.
 *
 * Note that the program is taken by value, so that any modifications
 * do not affect the original program.
 */
inline int run_amplifier(const std::vector<int>& program, int phase, int io)
{
    Process p {program};
    p.send_input(phase);
    p.send_input(io);
    p.join();
    int out;
    do
        out = p.get_output();
    while (p.output_count());
    return out;
}

int main()
{
    auto program = read_program("input.txt");

    int best = 0;
    std::array<int, 5> phases = {0, 1, 2, 3, 4};
    do {
        int io = 0;
        for (int p : phases)
            io = run_amplifier(program, p, io);
        best = std::max(best, io);
    }
    while (std::next_permutation(phases.begin(), phases.end()));

    std::cout << "Part 1: " << best << std::endl;
}
