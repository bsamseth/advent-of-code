#include <complex>
#include <fstream>
#include <iostream>
#include <stack>
#include <string>
#include <unordered_map>

using Point = std::complex<float>;

// Complex numbers not hashable by default. Only using them as 2D integers here,
// so a simple hash will suffice:
namespace std
{
template <>
struct hash<Point>
{
    std::size_t operator()(const Point& p) const
    {
        return ((std::hash<int>()((int) std::rint(p.real()))
                 ^ (std::hash<int>()((int) std::rint(p.imag())) << 1))
                >> 1);
    }
};
}  // namespace std

constexpr Point UP {0, 1};
constexpr Point DOWN {0, -1};
constexpr Point LEFT {-1, 0};
constexpr Point RIGHT {1, 0};
std::unordered_map<char, Point> Directions
    = {{'N', UP}, {'E', RIGHT}, {'S', DOWN}, {'W', LEFT}};



int main()
{
    std::ifstream infile("input.txt");
    std::string regex;
    infile >> regex;

    std::stack<Point> positions;
    Point p = {0, 0};
    Point p_prev = p;
    std::unordered_map<Point, int> dist {{p, 0}};

    for (int i = 1; i < (int) regex.size() - 1; ++i)
    {
        if (char c = regex[i]; c == '(')
            positions.push(p);
        else if (c == ')')
        {
            p = positions.top();
            positions.pop();
        }
        else if (c == '|')
            p = positions.top();
        else
        {
            p += 2.f * Directions[c];

            if (!dist.count(p))
                dist[p] = dist[p_prev] + 1;
            else
                dist[p] = std::min(dist[p], dist[p_prev] + 1);
        }
        p_prev = p;
    }

    int max_dist = -1;
    int more_than_999 = 0;
    for (auto [_, d] : dist)
    {
        max_dist = std::max(max_dist, d);
        if (d >= 1000)
            more_than_999++;
    }

    std::cout << "Part 1: " << max_dist << std::endl;
    std::cout << "Part 2: " << more_than_999 << std::endl;
}
