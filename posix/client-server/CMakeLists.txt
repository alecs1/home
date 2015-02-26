cmake_minimum_required (VERSION 2.6)
project(client-server)

SET (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-elide-constructors -fstack-protector-all -fstack-check -g -O0 -Wall -std=gnu11")

add_executable(server server.c)
target_link_libraries(server pthread m)


add_executable(client client.c)
target_link_libraries(client pthread)
