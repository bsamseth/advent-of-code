#include "intcode.hpp"

#include <algorithm>
#include <array>
#include <iostream>
#include <vector>

/* Run the program on an amplifier with a given phase and input, returning the output. */
inline int run_amplifier(const std::vector<int> &program, int phase, int io) {
    auto inputs = std::make_shared<IOQueue<int>>();
    auto outputs = std::make_shared<IOQueue<int>>();
    inputs->push(phase);
    inputs->push(io);

    Process p{program, inputs, outputs};
    p.join();
    return outputs->get_data().back();
}

int run_amplifier_with_feedback(const std::vector<int> &program,
                                const std::vector<int> &phases) {
    std::vector<std::shared_ptr<IOQueue<int>>> queues;
    queues.reserve(5);
    for (int i = 0; i < 5; ++i) {
        queues.push_back(std::make_shared<IOQueue<int>>());
        queues.back()->push(phases[i]);
    }

    std::vector<Process> procs;
    procs.reserve(5);
    for (int i = 0; i < 5; ++i)
        procs.emplace_back(program, queues[i], queues[(i + 1) % 5]);

    queues[0]->push(0);

    for (auto &proc : procs)
        proc.join();

    return queues[0]->pop();
}

int main() {
    auto program = read_program("input.txt");
    assert(program.size() && "Could not read program from file.");

    // Part 1.
    {
        int best = 0;
        std::array<int, 5> phases = {0, 1, 2, 3, 4};
        do {
            int io = 0;
            for (int p : phases)
                io = run_amplifier(program, p, io);
            best = std::max(best, io);
        } while (std::next_permutation(phases.begin(), phases.end()));

        std::cout << "Part 1: " << best << std::endl;
    }

    // Part 2.
    {
        int best = 0;
        std::vector<int> phases = {5, 6, 7, 8, 9};
        do {
            best = std::max(best, run_amplifier_with_feedback(program, phases));
        } while (std::next_permutation(phases.begin(), phases.end()));

        std::cout << "Part 2: " << best << std::endl;
    }
}
