#include "intcode.hpp"

#include <sstream>
#include <vector>
#include <algorithm>
#include <boost/multiprecision/cpp_int.hpp>

enum class Directions {
    UP, DOWN, LEFT, RIGHT
};

using Data = boost::multiprecision::cpp_int;  // It appears this didn't need big integers. (if needed:
// boost::multiprecision::cpp_int);
using Grid = std::vector<std::vector<char>>;
using Path = std::vector<Directions>;


void print_grid(const Grid& grid) {
    for (const auto& row : grid) {
        for (auto c : row) {
            std::cout << c;
        }
        std::cout << '\n';
    }
}

std::pair<int, int> find_robot(const Grid& grid) {
    for (int i = 0; i < (int) grid.size(); ++i)
        for (int j = 0; j < (int) grid[i].size(); ++j)
            if (grid[i][j] == '^')
                return {i, j};
    return {-1, -1};
}

int part2(Grid grid) {
    auto[y, x] = find_robot(grid);

    int dy = -1;
    int dx = 0;

    while (true) {

        bool done = true;
        for (int v = -1; v <= 1; v += 2) {
            if (v == -1) {
                int tmp = dx;
                dx = -dy * v;
                dy = tmp * v;
            } else {
                dx = -dx;
                dy = -dy;
            }

            int count = 0;
            while (y + dy >= 0 && y + dy < grid.size() && x + dx >= 0 && x + dx < grid[y + dy].size() &&
                   grid[y + dy][x + dx] != '.') {
                y += dy;
                x += dx;
                count++;
            }
            if (count) {
                std::cout << (v < 0 ? 'L' : 'R') << "," << count << "," << std::flush;
                done = false;
                break;
            }
        }

        if (done)
            break;
    }
    return 0;
}

int main() {
    // Part 1
    {
        Process<Data> process{"input.txt"};
        auto inputs = process.get_inputs();
        auto outputs = process.get_outputs();

        Grid grid{1, std::vector<char>{}};
        while (true) {
            auto maybe_output = outputs->pop_if_alive(process);

            if (!maybe_output.has_value())
                break;

            char c = (int) *maybe_output;

            if (c == '\n')
                grid.emplace_back();
            else
                grid.back().push_back(c);

//            std::cout << c;
        }

        int sum = 0;
        for (int i = 1; i + 3 < (int) grid.size(); ++i) {
            for (int j = 1; j + 1 < (int) grid[i].size(); ++j) {
                if (grid[i][j] == '#' && grid[i - 1][j] == '#' && grid[i + 1][j] == '#' && grid[i][j - 1] == '#' &&
                    grid[i][j + 1] == '#') {
                    sum += i * j;
                }
            }
        }
        std::cout << "Part 1: " << sum << std::endl;
        process.join();
    }

    // Part 2
    // To solve this, run this line first:
    // std::cout << "Path: " << part2(grid) << std::endl;
    // This prints the sequence of steps. Then compress this by hand. See compress.py for how this was done.
    // Then adjust the below hardcoded steps.
    {
        auto assembly = read_program<Data>("input.txt");
        assembly[0] = 2;
        Process<Data> process{assembly};
        auto inputs = process.get_inputs();
        auto outputs = process.get_outputs();

        for (char c : std::string{"A,C,A,B,A,B,C,B,B,C\n"})   // Main sequence
            inputs->push((int) c );
        for (char c : std::string{"L,4,L,4,L,10,R,4\n"})      // A
            inputs->push((int) c );
        for (char c : std::string{"R,4,L,10,R,10\n"})         // B
            inputs->push((int) c );
        for (char c : std::string{"R,4,L,4,L,4,R,8,R,10\n"})  // C
            inputs->push((int) c );

        inputs->push((int) 'n');  // No video feed required.
        inputs->push((int) '\n');

        // Let the process run its course, print only the last output (some grid mapping at first).
        Data last = 0;
        while (true) {
            auto maybe = outputs->pop_if_alive(process);
            if (!maybe.has_value())
                break;
            last = *maybe;
        }
        std::cout << "Part 2: " << last << std::endl;
        process.join();
    }
}
