project(FreeGo)
cmake_minimum_required(VERSION 2.8)

find_package(Qt5Bluetooth REQUIRED)

qt5_wrap_ui(UIS_HEADERS remoteselector.ui chat.ui)
qt5_wrap_cpp(remoteselector.h chat.h)
include_directories(${CMAKE_CURRENT_BINARY_DIR})
add_library(btchat remoteselector.cpp chatserver.cpp chatclient.cpp chat.cpp ${UIS_HEADERS} )
