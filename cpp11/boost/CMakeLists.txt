#SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
#SET (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -lboost_system")

SET (CMAKE_VERBOSE_MAKEFILE ON)


add_executable(asio-timer asio-timer.cpp)
target_link_libraries(asio-timer boost_system)
