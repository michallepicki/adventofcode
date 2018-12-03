#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include <unordered_map>
#include <unordered_set>

int main()
{
  int twos = 0, threes = 0;
  std::string id;
  std::ifstream input_file("../2");
  std::unordered_map<char, int> char_counts;
  std::unordered_set<int> counts;
  while (input_file >> id)
  {
    char_counts.clear();
    counts.clear();
    for (const char &c : id)
      char_counts[c]++;
    for (auto count : char_counts)
      counts.insert(count.second);
    if (counts.count(2))
      twos++;
    if (counts.count(3))
      threes++;
  }
  std::cout << twos * threes << std::endl;

  std::unordered_set<std::string> patterns;
  bool found = false;
  input_file.clear();
  input_file.seekg(0);
  while (!found && input_file >> id)
  {
    unsigned long max_pos = id.length() - 1;
    for (unsigned int i = 0; i < max_pos; i++)
    {
      auto result = patterns.insert(std::string(id).replace(i, 1, "_"));
      if (!result.second)
      {
        found = true;
        id = *(result.first);
        id.replace(i, 1, "");
      }
    }
  }
  std::cout << id << std::endl;
  return 0;
}
