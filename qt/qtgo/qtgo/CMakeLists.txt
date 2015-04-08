#nice documentation: https://qt-project.org/quarterly/view/using_cmake_to_build_qt_projects

project(FreeGo)
cmake_minimum_required(VERSION 2.8)

set (CMAKE_VERBOSE_MAKEFILE true)


find_package(Qt5Widgets REQUIRED)
find_package(Qt5Svg REQUIRED)
find_package(Qt5Quick REQUIRED)
find_package(Qt5Qml REQUIRED)
find_package(Qt5QuickWidgets REQUIRED)
include_directories(${Qt5Widgets_INCLUDE_DIRS})
include_directories(${Qt5Svg_INCLUDE_DIRS})
include_directories("../gnugo")
include_directories("../gnugo/sgf")
include_directories("../gnugo/utils")
#include_directories("../qtcurve/qt5")
#include_directories("../qtcurve/lib")

# Add compiler flags for building executables (-fPIE), required by Qt
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${Qt5Widgets_EXECUTABLE_COMPILE_FLAGS}")


#qt UI
qt5_wrap_ui(UIS_HEADERS mainwindow.ui GameSettings.ui PlayerWidget.ui GameEndDialog.ui ConfirmMoveDialog.ui RoundInfo.ui
    Handicap.ui HandicapDialog.ui)


#how to debug a CMake script:
message("UIS_HEADERS:" ${UIS_HEADERS})


#qt moc
qt5_wrap_cpp(HEADER_MOC  mainwindow.h GoTable.h GameSettings.h PlayerWidget.h GameEndDialog.h ConfirmMoveDialog.h RoundInfo.h
    Handicap.h HandicapDialog.h)

qt5_add_resources(RESOURCES_RCC res.qrc)

include_directories(${CMAKE_CURRENT_BINARY_DIR})
add_executable(FreeGo
    mainwindow.cpp GameStruct.cpp GoTable.cpp main.cpp GameSettings.cpp PlayerWidget.cpp GameEndDialog.cpp
    ConfirmMoveDialog.cpp RoundInfo.cpp SaveFile.cpp Global.cpp AboutDialog.cpp Handicap.cpp HandicapDialog.cpp
    ${HEADER_MOC} ${UIS_HEADERS} ${RESOURCES_RCC})

target_link_libraries(FreeGo Qt5::Widgets Qt5::Svg Qt5::Quick Qt5::QuickWidgets board engine utils sgf patterns)

add_custom_target(Doc Notes.txt)
