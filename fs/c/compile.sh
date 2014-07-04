#!/bin/bash

#TODO TODO TODO - move this to some Makefile or CMake or whatever

gcc -Wall -g -O0 -std=c99 basic_ops.c utils.c structures.c init_fs.c main.c -I. -I/usr/include -D_FILE_OFFSET_BITS=64 -o super_fs

#gcc -Wall -g -O0 -std=c99 main.c -linit
