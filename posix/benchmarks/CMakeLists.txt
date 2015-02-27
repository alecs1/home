cmake_minimum_required (VERSION 2.6)
project(benchmarks)

SET (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O3 -Wall -std=gnu11")

add_executable(create create.c)


add_executable(mutex_lock mutex_lock.c)
target_link_libraries(mutex_lock pthread)
