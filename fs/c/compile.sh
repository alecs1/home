#!/bin/bash

#TODO TODO TODO - move this to some Makefile or CMake or whatever

gcc -Wall -g -O0 -std=c99 init_fs.c main.c -I. -I/usr/include -o super_fs

#gcc -Wall -g -O0 -std=c99 main.c -linit
