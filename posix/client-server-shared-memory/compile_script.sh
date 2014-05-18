#!/bin/bash

#-D_POSIX_SOURCE is needed to compile with c99 and also be able to use struct addrinfo

echo "Compiling server"

c99 -g -lpthread -Wall -D_POSIX_SOURCE server.c -o server.exe
