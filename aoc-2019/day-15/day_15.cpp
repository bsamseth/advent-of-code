#include "intcode.hpp"

#include <algorithm>
#include <vector>
#include <deque>
#include <set>
#include <boost/multiprecision/cpp_int.hpp>

using Data = int;//boost::multiprecision::cpp_int;
using Point = std::pair<Data, Data>;

enum Status {
    WALL = 0, MOVED, FOUND
};

enum Direction {
    NORTH = 1, SOUTH = 2, WEST = 3, EAST = 4
};
using Path = std::vector<Direction>;

constexpr std::array<Direction, 4> directions{Direction::NORTH, Direction::SOUTH, Direction::WEST, Direction::EAST};

constexpr Direction operator~(const Direction d) {
    switch (d) {
        case Direction::NORTH:
            return Direction::SOUTH;
        case Direction::SOUTH:
            return Direction::NORTH;
        case Direction::WEST:
            return Direction::EAST;
        case Direction::EAST:
            return Direction::WEST;
        default:
            return Direction::NORTH;
    }
}

constexpr void update_position(std::pair<int, int>& point, Direction d) {
    switch (d) {
        case Direction::NORTH:
            point.second += 1;
            return;
        case Direction::SOUTH:
            point.second -= 1;
            return;
        case Direction::WEST:
            point.first -= 1;
            return;
        case Direction::EAST:
            point.first += 1;
            return;
    }
}

std::pair<bool, int>
find_oxygen_dfs(Process<Data>& droid, std::set<std::pair<int, int>>& seen, std::pair<int, int> position,
                int steps_so_far) {

    if (seen.count(position))
        return {false, steps_so_far};

    seen.insert(position);

    for (Direction d : directions) {
        droid.get_inputs()->push(Data{(int) d});
        Status s = static_cast<Status>((int) droid.get_outputs()->pop());

        if (s == Status::FOUND)
            return {true, steps_so_far + 1};
        if (s == Status::MOVED) {
            update_position(position, d);
            auto[found, steps] = find_oxygen_dfs(droid, seen, position, steps_so_far + 1);
            if (found)
                return {true, steps};

            // Undo movement.
            update_position(position, ~d);
            droid.get_inputs()->push(Data{(int) ~d});
            droid.get_outputs()->pop();
        }
    }
    return {false, steps_so_far};
}


int main() {
    auto assembly = read_program<Data>("input.txt");
    Process droid{assembly};

    std::set<std::pair<int, int>> seen;
    auto[found, steps] = find_oxygen_dfs(droid, seen, {0, 0}, 0);
    assert(found);
    std::cout << "Part 1: " << steps << std::endl;
    droid.join();
}
