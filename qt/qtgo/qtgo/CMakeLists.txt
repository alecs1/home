#nice documentation: https://qt-project.org/quarterly/view/using_cmake_to_build_qt_projects

project(FreeGo)
cmake_minimum_required(VERSION 2.8)

set (CMAKE_VERBOSE_MAKEFILE true)


find_package(Qt5Widgets)
find_package(Qt5Svg)
include_directories(${Qt5Widgets_INCLUDE_DIRS})
include_directories(${Qt5Svg_INCLUDE_DIRS})
include_directories("../gnugo")
include_directories("../gnugo/sgf")
include_directories("../gnugo/utils")


# Add compiler flags for building executables (-fPIE), required by Qt
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${Qt5Widgets_EXECUTABLE_COMPILE_FLAGS}")

if (${CMAKE_COMPILER_IS_GNUCXX})
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fstack-protector-all -fstack-check -D_GLIBCXX_DEBUG -g -O0 -Wall -std=c++11")
endif (${CMAKE_COMPILER_IS_GNUCXX})

#qt UI
qt5_wrap_ui(UIS_HEADERS mainwindow.ui GameSettings.ui PlayerWidget.ui GameEndDialog.ui ConfirmMoveDialog.ui RoundInfo.ui)


#how to debug a CMake script:
message("UIS_HEADERS:" ${UIS_HEADERS})


#qt moc
qt5_wrap_cpp(HEADER_MOC  mainwindow.h GoTable.h GameSettings.h PlayerWidget.h GameEndDialog.h ConfirmMoveDialog.h RoundInfo.h)

qt5_add_resources(RESOURCES_RCC res.qrc)

include_directories(${CMAKE_CURRENT_BINARY_DIR})
add_executable(FreeGo
    mainwindow.cpp GameStruct.cpp GoTable.cpp main.cpp GameSettings.cpp PlayerWidget.cpp GameEndDialog.cpp ConfirmMoveDialog.cpp RoundInfo.cpp SaveFile.cpp
    ${HEADER_MOC} ${UIS_HEADERS} ${RESOURCES_RCC})

#TODO - why don't I have to link libpatterns.a?
target_link_libraries(FreeGo Qt5::Widgets Qt5::Svg board engine utils sgf patterns)
