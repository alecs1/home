project(FreeGo)
cmake_minimum_required(VERSION 2.8)

find_package(Qt5Bluetooth REQUIRED)

add_library(btchat remoteselector.cpp chatserver.cpp chatclient.cpp chat.cpp)
