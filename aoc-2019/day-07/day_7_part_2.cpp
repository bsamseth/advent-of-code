#include "intcode.hpp"

#include <algorithm>
#include <array>
#include <iostream>
#include <memory>
#include <vector>

/*
 * Run the program on an amplifier with a given phase and input, returning the output.
 *
 * Note that the program is taken by value, so that any modifications
 * do not affect the original program.
 */
inline int run_amplifier(std::shared_ptr<Process> from,
                         std::shared_ptr<Process> me,
                         std::shared_ptr<Process> to)
{
    /* Process p {program}; */
    /* p.send_input(phase); */
    /* p.send_input(io); */
    /* p.join(); */
    /* int out; */
    /* do */
    /*     out = p.get_output(); */
    /* while (p.output_count()); */
    /* return out; */
    return 0;
}

int main()
{
    auto program = read_program("input.txt");

    int best = 0;
    std::array<int, 5> phases = {5, 6, 7, 8, 9};
    std::vector<std::shared_ptr<Process>> processes;
    for (int phase : phases)
    {
        auto process = std::make_shared<Process>(program);
        processes.push_back(process);
    }
    for (int i = 0; i < 5; ++i)
    {
        processes[i]->set_input_stream(processes[(i - 1) % 5]->outputs);
        processes[i]->set_output_stream(processes[(i + 1) % 5]->inputs);

        processes[(i - 1) % 5]->send_output(phases[i]);
    }

    processes[4]->send_output(0);

    for (auto proc : processes)
        proc->join();

    for (auto o : processes[4]->outputs)
        std::cout << o << " ";
    std::cout << std::endl;
    /* do { */
    /*     int io = 0; */
    /*     /1* for (int p : phases) *1/ */
    /*     /1*     io = run_amplifiers(program, p, io); *1/ */
    /*     best = std::max(best, io); */
    /* } */
    /* while (std::next_permutation(phases.begin(), phases.end())); */

    /* std::cout << "Part 2: " << best << std::endl; */
}
