#include "intcode.hpp"

#include <vector>
#include <boost/multiprecision/cpp_int.hpp>

using Data = int;//boost::multiprecision::cpp_int;  // It appears this didn't need big integers. (if needed:
// boost::multiprecision::cpp_int);

int main() {
    Process<Data> process{"input.txt"};
    auto inputs = process.get_inputs();
    auto outputs = process.get_outputs();

    std::vector<std::vector<char>> grid{1, std::vector<char>{}};
    while (true) {
        auto maybe_output = outputs->pop_if_alive(process);

        if (!maybe_output.has_value())
            break;

        char c = *maybe_output;

        if (c == '\n')
            grid.emplace_back();
        else
            grid.back().push_back(c);

        std::cout << c;
    }

    int sum = 0;
    for (int i = 1; i + 3 < (int) grid.size(); ++i) {
        for (int j = 1; j + 1 < (int) grid[i].size(); ++j) {
            if (grid[i][j] == '#' && grid[i - 1][j] == '#' && grid[i + 1][j] == '#' && grid[i][j - 1] == '#' && grid[i][j + 1] == '#') {
                sum += i * j;
                grid[i][j] = 'O';
            }
        }
    }
    for (const auto& row : grid) {
        for (auto c : row) {
            std::cout << c;
        }
        std::cout << '\n';
    }
    std::cout << "Part 1: " << sum << std::endl;
    process.join();
}
