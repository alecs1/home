#!/bin/bash
g++ -std=c++11 map.cpp -o map.exe

g++ -std=c++11 features.cpp -o features.exe

g++ -std=c++11 -g -O0 -Wall -fno-elide-constructors -fstack-protector-all -fstack-check features.cpp -o features2.exe


g++ -std=c++11 shared_ptr.cpp -o shared_ptr.exe