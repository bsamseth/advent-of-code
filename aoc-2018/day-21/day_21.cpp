#include <iostream>
#include <unordered_set>

int main()
{
    std::unordered_set<unsigned long long> values;
    unsigned long long last = 0;
    unsigned long long r[6] = {0, 0, 0, 0, 0, 0};

    // while ((123 & 456) != 72);  // No-op check.

    r[4] = 0;
i_06:
    r[1] = r[4] | 65536;
    r[4] = 678134;
i_08:
    r[5] = r[1] & 255;
    r[4] += r[5];
    r[4] &= 16777215;
    r[4] *= 65899;
    r[4] &= 16777215;

    if (256 > r[1])
    {
        // Added code: r[4] contains numbers that will cause halt.
        if (values.empty())
            std::cout << "Part 1: " << r[4] << '\n';
        else if (values.count(r[4]))
        {
            // Found first repeating number.
            // Assuming no new numbers will arise, so the last
            // value which would have cased a hlt is the one
            // that will leave the program running the longest while
            // still finishing.
            std::cout << "Part 2: " << last << '\n';
            return 0;
        }
        last = r[4];
        values.insert(last);
        // END of added code

        if (r[4] == r[0])
            return 0;
        else
            goto i_06;
    }
    else
    {
        r[1] = r[1] / 256;
        goto i_08;
    }
}
