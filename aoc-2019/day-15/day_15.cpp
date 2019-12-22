#include "intcode.hpp"

#include <algorithm>
#include <vector>
#include <deque>
#include <set>
#include <boost/multiprecision/cpp_int.hpp>

using Data = boost::multiprecision::cpp_int;
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


template<bool Reverse, typename It>
auto do_walk(Process<Data>& droid, It begin, It end) {
    auto command_stream = droid.get_inputs();
    auto status_stream = droid.get_outputs();

    std::pair<int, int> point {0, 0};

    Status s = Status::WALL;
    for (It it = begin; it != end; ++it) {
        Direction d = Reverse ? ~(*it) : *it;

        switch (d) {
            case Direction::NORTH: point.second += 1; break;
            case Direction::SOUTH: point.second -= 1; break;
            case Direction::WEST: point.first -= 1; break;
            case Direction::EAST: point.first += 1; break;
        }

        command_stream->push(Data{(int) d});
        s = static_cast<Status>((int) status_stream->pop());
    }
    return std::pair<Status, std::pair<int, int>> {s, point};
}

inline auto forward(Process<Data>& droid, const Path& path) {
    return do_walk<false>(droid, path.begin(), path.end());
}

inline auto reverse(Process<Data>& droid, const Path& path) {
    return do_walk<true>(droid, path.rbegin(), path.rend());
}

Path find_oxygen(Process<Data>& droid) {
    std::deque<Path> paths;
    for (Direction dir : directions)
        paths.emplace_back(1, dir);

    std::set<std::pair<int, int>> seen;

    while (!paths.empty()) {
        Path path = paths.front();
        paths.pop_front();

        auto [s, where] = forward(droid, path);

        if (s == Status::WALL) {
            path.pop_back();
            reverse(droid, path);
            continue;
        }

        reverse(droid, path);

        if (s == Status::FOUND)
            return path;
        if (seen.count(where))
            continue;

        seen.insert(where);

        for (Direction dir : directions)
        {
            if (dir != ~path.back()) {
                path.push_back(dir);
                paths.push_back(path);
                path.pop_back();
            }
        }
    }

    return Path{};
}

//Path find_oxygen_dfs(Process<Data>& droid, std::set<std::pair<int, int>>& seen) {
//
//}



int main() {
    auto assembly = read_program<Data>("input.txt");
    auto inputs = std::make_shared<IOQueue<Data>>();
    auto outputs = std::make_shared<IOQueue<Data>>();
    Process droid{assembly, inputs, outputs};
    Path path_to_oxygen = find_oxygen(droid);
    std::cout << "Part 1: " << path_to_oxygen.size() << std::endl;

//    int i = 0;
//    while (i >= 0) {
//        std::cout << "Next direction: ";
//        std::cin >> i;
//        inputs->push(Data{i});
//        std::cout << "Status: " << outputs->pop() << std::endl;
//    }

    droid.join();
}
