#/usr/bin/env sh
clang++-6.0 -Wextra -std=c++17 $1 && ./a.out && rm a.out
