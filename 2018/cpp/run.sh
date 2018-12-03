#/usr/bin/env sh
clang++-6.0 -Weverything -Wno-c++98-compat -std=c++17 $1 && ./a.out && rm a.out
