cmake_minimum_required (VERSION 2.6) 
project (cpp11)

SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-elide-constructors -fstack-protector-all -fstack-check -D_GLIBCXX_DEBUG -g -O0 -Wall -std=c++11")
#SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -std=c++11")

add_subdirectory(boost)

add_executable(map map.cpp)
add_executable(features features.cpp)
add_executable(shared_ptr shared_ptr.cpp)
add_executable(perfect-forwarding perfect-forwarding.cpp)
add_executable(lambda lambda.cpp)
add_executable(lambda-complex lambda-complex.cpp)
add_executable(decltype-retvalsyntax decltype-retvalsyntax.cpp)
add_executable(unique_ptr unique_ptr.cpp) 


