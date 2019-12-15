#include "intcode.hpp"

#include <iostream>
#include <memory>
#include <map>
#include <boost/multiprecision/cpp_int.hpp>

using Color = bool;
using Data = boost::multiprecision::cpp_int;
using Point = std::pair<int, int>;

inline auto read(const std::map<Point, Color>& grid, Point where) {
    auto it = grid.find(where);
    return it != grid.end() && it->second;
}

std::map<Point, Color> paint(Color starting_color = false) {
    auto inputs = std::make_shared<IOQueue<Data>>();
    auto outputs = std::make_shared<IOQueue<Data>>();

    Process<Data> brain{read_program<Data>("input.txt"), inputs, outputs};

    std::map<Point, Color> grid;
    Point pos{0, 0};
    Point dir{0, 1};

    grid[pos] = starting_color;

    while (true) {
        inputs->push(read(grid, pos));
        auto color = outputs->pop_if_alive(brain);

        if (!color.has_value())
            break;

        grid[pos] = (Color) *color;
        dir = outputs->pop() ? Point{dir.second, -dir.first} : Point{-dir.second, dir.first};
        pos = Point{pos.first + dir.first, pos.second + dir.second};
    }
    brain.join();
    return grid;
}

int main() {
    std::cout << "Part 1: " << paint().size() << std::endl;
    std::cout << "Part 2:\n";

    // Get the bounding boxes of the grid.
    auto grid = paint(true);
    Point xlim{0, 0};
    Point ylim{0, 0};
    for (const auto[pos, _] : grid) {
        xlim.first = std::min(xlim.first, pos.first);
        xlim.second = std::max(xlim.second, pos.first);
        ylim.first = std::min(ylim.first, pos.second);
        ylim.second = std::max(ylim.second, pos.second);
    }

    // Write painted area to terminal:
    for (int y = ylim.second; y >= ylim.first; --y) {
        for (int x = xlim.first; x <= xlim.second; ++x) {
            std::cout << (grid[Point{x, y}] ? "\u2588" : " ");
        }
        std::cout << '\n';
    }
}
