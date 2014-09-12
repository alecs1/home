project(distr-ip)
cmake_minimum_required(VERSION 2.8)
aux_source_directory(. SRC_LIST)

#add_executable(${PROJECT_NAME} ${SRC_LIST})

#Windows only stuff
include_directories("C:\\local\\boost_1_56_0")
link_directories("C:\\local\\boost_1_56_0\\lib64-msvc-12.0")
set (CMAKE_EXE_LINKER_FLAGS " /machine:X64 ")
set (CMAKE_SHARED_LINKER_FLAGS " /machine:X64 ")
#end Windows only stuff


add_library(tgafunctions SHARED TGA.cpp TGA.h)

add_executable(distr-ip main.cpp global_defines.h definitions.h)
target_link_libraries(distr-ip tgafunctions)

add_executable(worker-client worker-client.cpp global_defines.h definitions.h)
target_link_libraries(worker-client tgafunctions)



#a couple of test programs
add_executable(example-server example-server.cpp)
add_executable(example-server2 example-server2.cpp)
add_executable(fs-ops fs-ops.cpp)
#target_link_libraries(fs-ops boost_system boos_filesystem)
