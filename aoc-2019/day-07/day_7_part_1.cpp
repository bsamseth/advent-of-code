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
inline int run_amplifier(std::vector<int> program, int phase, int io)
{
    int ip = 0;
    // Execute the first input instruction, with phase as IO.
    execute_inst(program, Opcode{program[ip++]}, ip, phase);

    // Now execute the remaining program with input set to io.
    while (execute_inst(program, Opcode {program[ip++]}, ip, io)) {}
    return io;
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
