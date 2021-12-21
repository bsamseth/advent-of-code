#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <cstdlib>

int play_till_round(const std::vector<int>& init_numbers, int max_round) {
    std::vector<int> memory(max_round, 0);
    for (int i = 0; i < init_numbers.size() - 1; ++i)
        memory[init_numbers[i]] = i+1;

    int last_number = init_numbers.back();
    int next_number;
    for (int round_nr = init_numbers.size() + 1; round_nr <= max_round; ++round_nr) {
        int from_memory = memory[last_number];
        next_number = from_memory ? round_nr - 1 - from_memory : 0;
        memory[last_number] = round_nr - 1;
        last_number = next_number;
    }
    return next_number;
}

int main() {
    std::ifstream input ("input.txt");
    std::vector<int> init_numbers;
    std::string s;
    while (std::getline(input, s, ','))
        init_numbers.push_back(std::atoi(s.c_str()));

    std::cout << "Part 1: " << play_till_round(init_numbers, 2020) << '\n';
    std::cout << "Part 2: " << play_till_round(init_numbers, 30000000) << '\n';
}
