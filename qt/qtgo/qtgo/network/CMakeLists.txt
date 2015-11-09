project(FreeGo)
cmake_minimum_required(VERSION 2.8)

qt5_wrap_ui(UIS_HEADERS BTErrorDialog.ui)
qt5_wrap_cpp(MOC_HEADERS BTServer.h BTClient.h BTErrorDialog.h)

add_library(network Proto.cpp ProtoJson.cpp BTServer.cpp BTClient.cpp ConnMan.cpp BTErrorDialog.cpp ${UIS_HEADERS} ${MOC_HEADERS} )

add_custom_target(Doc json-protcol.txt)
