#include <fstream>
#include <iostream>
#include <stack>
#include <unordered_map>
#include <utility>
#include <vector>

// Point structure with 'reading order' sorting:
struct Point
{
    int x;
    int y;

    constexpr std::pair<int, int> asPair() const { return std::pair<int, int> {y, x}; }
};

constexpr bool operator==(const Point& a, const Point& b)
{
    return a.x == b.x && a.y == b.y;
}

constexpr Point operator+(const Point& a, const Point& b)
{
    return Point {a.x + b.x, a.y + b.y};
}

namespace std
{
template <>
struct hash<Point>
{
    std::size_t operator()(const Point& p) const
    {
        return ((std::hash<int>()(p.x) ^ (std::hash<int>()(p.y) << 1)) >> 1);
    }
};

}  // namespace std

std::ostream& operator<<(std::ostream& stm, const Point& p)
{
    return stm << "(" << p.x << ", " << p.y << ")";
}

constexpr Point UP {0, -1};
constexpr Point DOWN {0, 1};
constexpr Point LEFT {-1, 0};
constexpr Point RIGHT {1, 0};
constexpr std::array<Point, 4> Directions = {UP, DOWN, LEFT, RIGHT};

// END of Point definitions

// Read problem

struct ProblemDefinition
{
    int min_y;
    int max_y;
    int min_x;
    int max_x;
    std::unordered_map<Point, char> grid;

    ProblemDefinition(std::string filename)
    {
        struct Scan
        {
            int x1, x2, y1, y2;
        };

        std::ifstream infile {filename};
        char g, d1;
        std::vector<Scan> scans;
        Scan cur;
        min_y = 1000000;
        max_y = 0;
        min_x = 500;
        max_x = 500;

        while (infile >> d1 >> g >> cur.x1 >> g >> g >> g >> cur.y1 >> g >> g >> cur.y2)
        {
            cur.x2 = cur.x1;
            if (d1 == 'y')
            {
                std::swap(cur.x1, cur.y1);
                std::swap(cur.x2, cur.y2);
            }
            scans.push_back(cur);
            min_y = std::min(min_y, cur.y1);
            max_y = std::max(max_y, cur.y2);
            min_x = std::min(min_x, cur.x1);
            max_x = std::max(max_x, cur.x2);
        }

        for (Scan s : scans)
            for (int x = s.x1; x <= s.x2; x++)
                for (int y = s.y1; y <= s.y2; y++)
                    grid[Point {x, y}] = '#';
    }
};

// Print and counts squares.
std::pair<int, int> print(const ProblemDefinition& problem)
{
    int tot = 0;
    int wat = 0;
    auto [min_y, max_y, min_x, max_x, grid] = problem;
    for (int y = 0; y <= max_y; y++)
    {
        for (int x = min_x - 2; x <= max_x + 2; x++)
        {
            Point p = Point {x, y};
            if (auto it = grid.find(p); it != grid.end())
            {
                if (y >= min_y)
                {
                    if (it->second == '|' || it->second == '~')
                        tot++;
                    if (it->second == '~')
                        wat++;
                }
                std::cout << it->second;
            }
            else
                std::cout << ".";
        }
        std::cout << "\n";
    }
    std::cout << "\n";
    return {tot, wat};
}

int main()
{
    ProblemDefinition problem {"input.txt"};
    auto& [min_y, max_y, min_x, max_x, grid] = problem;

    grid[Point {500, 0}] = '|';  // Water spring.
    std::stack<Point> queue;
    queue.push(Point {500, 0});  // emplace(500, 0);

    auto set_square = [&](const Point& p, char c) {
        if (p.y > max_y)
            return;

        if (auto it = grid.find(p); it != grid.end() && it->second == '#')
            return;
        else if (it == grid.end() || it->second != c)
        {
            for (Point d : Directions)
                queue.push(p + d);
            queue.push(p);
        }
        grid[p] = c;
    };

    while (queue.size() > 0)
    {
        Point p = queue.top();
        queue.pop();

        if (p.y > max_y)
            continue;

        if (auto it = grid.find(p); it != grid.end())
        {
            auto [_, c] = *it;

            if (c == '#')
                continue;

            // Check if we should make it still water:
            else if (c == '~')
            {
                set_square(p + LEFT, '~');
                set_square(p + RIGHT, '~');
            }

            else if (c == '|')
            {
                // Flow down?
                if (auto it = grid.find(p + DOWN); it == grid.end())
                    set_square(p + DOWN, '|');

                // Flow out?
                else if (auto [_, under] = *it; under == '~' || under == '#')
                {
                    set_square(p + LEFT, '|');
                    set_square(p + RIGHT, '|');

                    bool supported = true;
                    for (int dir = -1; dir <= 1 && supported; dir += 2)
                    {
                        for (int dx = 0; supported; dx++)
                        {
                            Point check {p.x + dir * dx, p.y};
                            if (auto it = grid.find(check);
                                it != grid.end() && it->second == '#')
                                break;

                            if (auto it = grid.find(check + DOWN);
                                it == grid.end() || it->second == '|')
                                supported = false;
                        }
                    }
                    if (supported)
                        set_square(p, '~');
                }
            }
        }
    }

    auto [total_wet, still_water] = print(problem);
    std::cout << "Total wet: " << total_wet << '\n';
    std::cout << "Still water: " << still_water << '\n';
    std::cout << max_y << std::endl;
}
