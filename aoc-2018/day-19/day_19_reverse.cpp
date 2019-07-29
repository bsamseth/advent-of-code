#include <iostream>

int main()
{
    // Loop over input setup (part 1 first, then part 2)
    for (int r0 = 0; r0 <= 1; ++r0)
    {

        long long r[6] = {r0, 0, 0, 0, 0, 0};

        if (r[0] == 0)
            r[1] = 998;
        else
            r[1] = 10551398;

        r[0] = 0;
        r[4] = 1;
        do
        {
            r[2] = 1;
            do
            {
                if (r[4] * r[2] == r[1])
                    r[0] += r[4];
                r[2]++;
            } while (r[4] * r[2] <= r[1]);  // Changed condition from r[2] <= r[1], only
                                            // algorithmic change.

            r[4]++;
        } while (r[4] <= r[1]);

        std::cout << "Part " << r0+1 << ": " << r[0] << std::endl;
    }
}
