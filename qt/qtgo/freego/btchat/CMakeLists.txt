project(FreeGo)
cmake_minimum_required(VERSION 2.8)

find_package(Qt5Bluetooth REQUIRED)
find_package(Qt5Widgets REQUIRED)

qt5_wrap_ui(UI_HEADERS_BTCHAT remoteselector.ui chat.ui)
qt5_wrap_cpp(MOC_HEADERS remoteselector.h chat.h chatclient.h chatserver.h)
include_directories(${CMAKE_CURRENT_BINARY_DIR})
add_library(btchat remoteselector.cpp chatserver.cpp chatclient.cpp chat.cpp ${UI_HEADERS_BTCHAT} ${MOC_HEADERS})
