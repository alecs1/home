project(distr-ip)
cmake_minimum_required(VERSION 2.8)
aux_source_directory(. SRC_LIST)

set(Boost_USE_STATIC_LIBS        OFF)
set(Boost_USE_MULTITHREADED      ON)

#platform specific stuff
IF (${CMAKE_SYSTEM_NAME} MATCHES "Win")
	set (BOOST_ROOT "C:\\local\\boost_1_56_0")
	set (BOOST_LIBRARYDIR "C:\\local\\boost_1_56_0\\lib64-msvc-12.0")
    include_directories("C:\\local\\boost_1_56_0")
    link_directories("C:\\local\\boost_1_56_0\\lib64-msvc-12.0")
    set (CMAKE_EXE_LINKER_FLAGS " /machine:X64 ")
    set (CMAKE_SHARED_LINKER_FLAGS " /machine:X64 ")
    #set (POSIX_REALTIME_EXTENSION_LIBRARY "")
ENDIF (${CMAKE_SYSTEM_NAME} MATCHES "Win")

IF (${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    set (POSIX_REALTIME_EXTENSION_LIBRARY "rt")
ENDIF (${CMAKE_SYSTEM_NAME} MATCHES "Linux")


IF (${CMAKE_COMPILER_IS_GNUCXX})
    SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-elide-constructors -fstack-protector-all -fstack-check -D_GLIBCXX_DEBUG -g -O0 -Wall -std=c++11")
ENDIF (${CMAKE_COMPILER_IS_GNUCXX})
#platform specific stuff-end


find_package( Boost REQUIRED COMPONENTS system filesystem thread iostreams coroutine context)


add_library(tgafunctions SHARED TGA.cpp TGA.h)

add_executable(distr-ip main.cpp global_defines.h definitions.h TGA.h distr-ip.h)
target_link_libraries(distr-ip tgafunctions ${Boost_SYSTEM_LIBRARY} ${Boost_FILESYSTEM_LIBRARY} ${Boost_THREAD_LIBRARY} ${Boost_IOSTREAMS_LIBRARY})


add_executable(worker-client worker-client.cpp global_defines.h definitions.h TGA.h)
target_link_libraries(worker-client tgafunctions ${Boost_SYSTEM_LIBRARY} ${Boost_IOSTREAMS_LIBRARY}) #iostreams dragged in by tgafunctions


#a couple of test programs
add_executable(example-server example-server.cpp)
target_link_libraries(example-server ${Boost_SYSTEM_LIBRARY} ${Boost_CONTEXT_LIBRARY} ${Boost_COROUTINE_LIBRARY})
add_executable(example-server2 example-server2.cpp)
target_link_libraries(example-server2 ${Boost_SYSTEM_LIBRARY} ${Boost_THREAD_LIBRARY})
add_executable(fs-ops fs-ops.cpp)
target_link_libraries(fs-ops ${Boost_SYSTEM_LIBRARY} ${Boost_FILESYSTEM_LIBRARY})
#target_link_libraries(fs-ops boost_system boos_filesystem)
