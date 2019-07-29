#include <cstring>  // for memcpy
#include <fstream>
#include <iostream>

constexpr int N = 50;
char acre[N][N];
char acre_copy[N][N];

int main()
{
    // READ problem
    std::ifstream infile {"input.txt"};
    for (int i = 0; i < N; ++i)
    {
        for (int j = 0; j < N; ++j)
        {
            infile >> acre[i][j];
        }
    }
    std::memcpy(acre_copy, acre, N * N);

    int prev = -1;
    int prev_min = -1;
    int target = -1;

    for (int min = 1; true; ++min)
    {
        int total_wood = 0;
        int total_lumber = 0;
        for (int i = 0; i < N; ++i)
        {
            for (int j = 0; j < N; ++j)
            {
                if (acre[i][j] == '|')
                    ++total_wood;
                else if (acre[i][j] == '#')
                    ++total_lumber;

                // Evolve:
                int neighbor_wood = 0;
                int neighbor_lumber = 0;
                for (int di = -1; di <= 1; ++di)
                {
                    for (int dj = -1; dj <= 1; ++dj)
                    {
                        if ((di || dj)
                            && (i + di >= 0 && i + di < N && j + dj >= 0 && j + dj < N))
                        {
                            char n = acre[i + di][j + dj];
                            if (n == '|')
                                ++neighbor_wood;
                            else if (n == '#')
                                ++neighbor_lumber;
                        }
                    }
                }
                if (acre[i][j] == '.' && neighbor_wood >= 3)
                    acre_copy[i][j] = '|';
                else if (acre[i][j] == '|' && neighbor_lumber >= 3)
                    acre_copy[i][j] = '#';
                else if (acre[i][j] == '#'
                         && !(neighbor_wood >= 1 && neighbor_lumber >= 1))
                    acre_copy[i][j] = '.';
            }
        }
        memcpy(acre, acre_copy, N * N);

        if (min == 10)
            std::cout << "Part 1: " << total_wood * total_lumber << std::endl;

        else if (min == 1000)  // Sufficiently large to enter periodic state.
        {
            prev = total_wood * total_lumber;  // Mark to detect period.
            prev_min = min;
        }
        else if (prev == total_wood * total_lumber)  // One period complete?
        {
            target = min + (1000000000 - min) % (min - prev_min) + 1;
        }
        else if (min == target)
        {
            std::cout << "Part 2: " << total_wood * total_lumber << std::endl;
            break;
        }
    }
}
