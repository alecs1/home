project(distr-ip)
cmake_minimum_required(VERSION 2.8)
aux_source_directory(. SRC_LIST)

#add_executable(${PROJECT_NAME} ${SRC_LIST})

include_directories("C:\\local\\boost_1_56_0")
link_directories("C:\\local\\boost_1_56_0\\lib64-msvc-12.0")

set (CMAKE_EXE_LINKER_FLAGS " /machine:X64 ")

add_executable (distr-ip main.cpp)

add_executable(example-server example-server.cpp)
add_executable (example-server2 example-server2.cpp)