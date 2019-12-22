#include "intcode.hpp"

#include <deque>

using Data = int;  // It appears this didn't need big integers. (if needed:
                   // boost::multiprecision::cpp_int);

enum Status
{
    MOVED = 1,
    FOUND
};

enum Direction : unsigned
{
    NORTH = 1,
    SOUTH = 2,
    WEST = 3,
    EAST = 4,
    NO_DIRECTION = 0
};

constexpr std::array<Direction, 4> directions {Direction::NORTH,
                                               Direction::SOUTH,
                                               Direction::WEST,
                                               Direction::EAST};

constexpr Direction operator~(const Direction d)
{
    return static_cast<Direction>(d & 1 ? d + 1 : d - 1);
}

inline Status move(Process<Data>& droid, Direction d)
{
    droid.get_inputs()->push(Data {(int) d});
    return static_cast<Status>((int) droid.get_outputs()->pop());
}

std::pair<bool, int> find_oxygen_dfs(Process<Data>& droid,
                                     int steps_so_far = 0,
                                     Direction last_dir = Direction::NO_DIRECTION)
{
    int max_steps_seen = steps_so_far;

    for (Direction d : directions)
    {
        if (d == ~last_dir)
            continue;

        Status s = move(droid, d);

        if (s == Status::FOUND)
            return {true, steps_so_far + 1};

        if (s == Status::MOVED)
        {
            auto [found, steps] = find_oxygen_dfs(droid, steps_so_far + 1, d);
            if (found)
                return {true, steps};

            max_steps_seen = std::max(max_steps_seen, steps);
            move(droid, ~d);
        }
    }
    return {false, max_steps_seen};
}

int main()
{
    Process<Data> droid {"input.txt"};

    auto [found, steps] = find_oxygen_dfs(droid);
    assert(found);
    std::cout << "Part 1: " << steps << std::endl;

    // Droid now at the oxygen. Doing another search will fail, and return
    // the maximum number of steps taken along any path (aka. the answer).
    auto [_, minutes] = find_oxygen_dfs(droid);
    std::cout << "Part 2: " << minutes << std::endl;

    droid.join();
}
