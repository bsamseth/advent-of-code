#include <fstream>
#include <iostream>
#include <queue>
#include <string>
#include <tuple>
#include <unordered_map>
#include <vector>

using Point = std::pair<int, int>;
using Time = int;
using Equipment = int;
using State = std::tuple<Time, Time, Point, Equipment>;
using VisitedEntry = std::tuple<Point, Equipment>;

constexpr Point Directions[4] = {{1, 0}, {0, 1}, {-1, 0}, {0, -1}};

// Need to define hash function for Point and VisitedEntry to use unordered_map.
// The hash can be simple due to the limited domain of the numbers. This saves
// a lot of time compared to using std::hash<int> in some combination.

namespace std
{
template <>
struct hash<Point>
{
    std::size_t operator()(const Point& p) const
    {
        return (p.first << (sizeof(int) * 4)) ^ p.second;
    }
};
template <>
struct hash<VisitedEntry>
{
    std::size_t operator()(const VisitedEntry& v) const
    {
        return (std::hash<Point>()(std::get<0>(v)) << 2) ^ std::get<1>(v);
    }
};
}  // namespace std

// Variables to be read from input.
int depth, tx, ty;
Point target;

int erosion(const Point& pos)
{
    static std::unordered_map<Point, int> erosion_memory;

    if (auto it = erosion_memory.find(pos); it != erosion_memory.end())
        return it->second;

    auto [x, y] = pos;
    int geoindex;
    if (pos == target)
        geoindex = 0;
    else if (y == 0)
        geoindex = x * 16807;
    else if (x == 0)
        geoindex = y * 48271;
    else
        geoindex = erosion({x, y - 1}) * erosion({x - 1, y});

    int e = (geoindex + depth) % 20183;
    erosion_memory[pos] = e;
    return e;
}

inline int type(const Point& pos)
{
    return erosion(pos) % 3;
}

// Consistent and admissable estimate of remaining time to target.
inline int heuristic(const Point& pos, Equipment eq)
{
    return std::abs(tx - pos.first) + std::abs(ty - pos.second) + (eq == 1 ? 0 : 7);
}

int main()
{
    std::ifstream infile {"input.txt"};
    std::string ignore_word;
    char ignore_char;

    infile >> std::skipws >> ignore_word >> depth >> ignore_word >> tx >> ignore_char
        >> ty;
    target = Point {tx, ty};

    std::cout << depth << " " << tx << " " << ty << '\n';

    int risk = 0;
    for (int y = 0; y <= ty; ++y)
        for (int x = 0; x <= tx; ++x)
            risk += type({x, y});

    std::cout << "Part 1: " << risk << '\n';

    // Part 2: A* search from {0, 0} to target.
    std::priority_queue<State, std::vector<State>, std::greater<State>> queue;
    queue.push({heuristic({0, 0}, 1),  // estimate
                0,                     // time
                {0, 0},                // pos
                1});                   // equipment = torch

    std::unordered_map<VisitedEntry, Time> visited;
    visited[{{0, 0}, 1}] = 0;  // (pos, equipment = torch) : time.

    while (true)
    {
        auto [_, time, pos, equipment] = queue.top();
        queue.pop();

        if (pos == target && equipment == 1)
        {
            std::cout << "Part 2: " << time << '\n';
            break;
        }

        auto next = [&](Time t, Equipment eq) {
            for (auto dir : Directions)
            {
                Point n = {pos.first + dir.first, pos.second + dir.second};
                // Illegal position, or illegal equipment for this neighbor?
                if (n.first < 0 || n.second < 0 || type(n) == eq)
                    continue;
                // Been here before from a shorter path?
                if (auto it = visited.find({n, eq});
                    it != visited.end() && it->second <= t)
                    continue;

                visited[{n, eq}] = t;
                queue.emplace(t + heuristic(n, eq), t, n, eq);
            }
        };

        next(time + 1, equipment);                       // Move with same equipment.
        next(time + 8, (type(pos) | equipment) ^ 0b11);  // Switch.
    }
}
