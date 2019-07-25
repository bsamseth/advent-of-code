#include <fstream>
#include <iostream>
#include <vector>
#include <algorithm>

int main() {
  std::ifstream input_file("input.txt");
  unsigned input_number;
  input_file >> input_number;
  input_file.close();

  // Build list of digits from number:
  std::vector<unsigned> numbers;
  for (unsigned n = input_number; n > 0; n /= 10)
    numbers.push_back(n % 10);
  std::reverse(numbers.begin(), numbers.end());

  // Part 1 & 2:
  unsigned elf1 = 0;
  unsigned elf2 = 1;
  bool part_2_done = false;
  std::vector<unsigned> recepies = {3, 7};
  unsigned index = 0;

  while (recepies.size() < input_number + 10 || !part_2_done) {
    unsigned r1 = recepies[elf1];
    unsigned r2 = recepies[elf2];
    unsigned sum = r1 + r2;
    if (sum >= 10) {
      recepies.push_back(1);
      sum -= 10;
    }
    recepies.push_back(sum);

    elf1 = (elf1 + 1 + r1) % recepies.size();
    elf2 = (elf2 + 1 + r2) % recepies.size();

    // Check for part two:
    unsigned i = 0;
    while (index + i < recepies.size()) {
      if (recepies[index + i] == numbers[i]) {
        if (i + 1 == numbers.size()) {
          part_2_done = true;
          break;
        }
        ++i;
      }
      else {
        i = 0;
        ++index;
      }
    }
  }

  // Build answer to part 1:
  long answer = 0;
  for (unsigned i = input_number; i < input_number + 10; ++i) {
    answer *= 10;
    answer += recepies[i];
  }

  std::cout << "Part 1: " << answer << std::endl;
  std::cout << "Part 2: " << index << std::endl;

}
