#!/bin/bash

#-D_POSIX_SOURCE is needed to compile with c99 and also be able to use struct addrinfo

echo "Compiling server"
#c99 -g -lpthread -Wall -D_POSIX_SOURCE server.c -o server.exe
gcc -std=gnu99 -g -lpthread -lm -Wall -D_POSIX_SOURCE server.c -o server.exe


echo "Compiling client"
#c99 -g -Wall -D_POSIX_SOURCE client.c -o client.exe
gcc -std=gnu99 -g -lpthread -Wall -DPOSIX_SOURCE client.c -o client.exe
