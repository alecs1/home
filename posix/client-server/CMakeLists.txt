cmake_minimum_required (VERSION 2.6)
project(client-server)

SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-elide-constructors -fstack-protector-all -fstack-check -D_GLIBCXX_DEBUG -g -O0 -Wall -std=c++11")

add_executable(server server.c)
add_executable(client client.c)
