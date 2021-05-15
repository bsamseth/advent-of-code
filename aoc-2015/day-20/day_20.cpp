#include <cmath>
#include <fstream>
#include <functional>
#include <iostream>

int part1_present_sum(const int house)
{
    const int d = std::sqrt(house) + 1;
    int s = 0;
    for (int i = 1; i <= d; i++)
        if (house % i == 0)
            s += i + house / i;
    return 10 * s;
}

int part2_present_sum(const int house)
{
    const int d = std::sqrt(house) + 1;
    int s = 0;
    for (int i = 1; i <= d; i++)
        if (house % i == 0)
            s += (i <= 50 ? house / i : 0) + (house / i <= 50 ? i : 0);
    return 11 * s;
}

int find_first_house(const int target, const std::function<int(int)>& present_function)
{
    for (int n = 1;; n++)
        if (present_function(n) >= target)
            return n;
}

int main()
{
    std::ifstream input_file {"input.txt"};
    int target;
    input_file >> target;

    std::cout << "Part 1: " << find_first_house(target, part1_present_sum) << std::endl;
    std::cout << "Part 2: " << find_first_house(target, part2_present_sum) << std::endl;
}
