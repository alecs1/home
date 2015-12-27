project(FreeGo)
cmake_minimum_required(VERSION 2.8)

find_package(Qt5Network REQUIRED)

qt5_wrap_ui(UI_HEADERS_NETWORK BTErrorDialog.ui)
qt5_wrap_cpp(MOC_HEADERS BTServer.h BTClient.h BTErrorDialog.h)

add_library(network Proto.cpp ProtoJson.cpp BTServer.cpp BTClient.cpp ConnMan.cpp BTErrorDialog.cpp ${UI_HEADERS_NETWORK} ${MOC_HEADERS} )

add_custom_target(Doc json-protcol.txt)
