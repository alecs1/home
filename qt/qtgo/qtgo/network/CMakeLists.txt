project(FreeGo)
cmake_minimum_required(VERSION 2.8)

qt5_wrap_cpp(MOC_HEADERS BTServer.h BTClient.h)

add_library(network Proto.cpp ProtoJson.cpp BTServer.cpp BTClient.cpp ConnMan.cpp ${MOC_HEADERS})

add_custom_target(Doc json-protcol.txt)
