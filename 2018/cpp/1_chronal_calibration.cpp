#include <iostream>
#include <fstream>
#include <unordered_set>
#include <utility>

int main()
{
  int freq_change, freq = 0;
  std::ifstream input_file("../1");
  while (input_file >> freq_change)
    freq += freq_change;
  std::cout << freq << std::endl;

  freq = 0;
  bool found = false;
  std::unordered_set<int> reached_freqs = {0};
  while (!found)
  {
    input_file.clear();
    input_file.seekg(0);
    while (!found && input_file >> freq_change){
      freq += freq_change;
      auto result = reached_freqs.insert(freq);
      if(!result.second)
        found = true;
    }
  }
  std::cout << freq << std::endl;
  return 0;
}
