#nice documentation: https://qt-project.org/quarterly/view/using_cmake_to_build_qt_projects

project(qtgo)
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
qt5_wrap_ui(UIS_HEADERS mainwindow.ui GameSettings.ui)


#how to debug a CMake script:
message("UIS_HEADERS:" ${UIS_HEADERS})


#qt moc
qt5_wrap_cpp(HEADER_MOC GoTable.h mainwindow.h)

include_directories(${CMAKE_CURRENT_BINARY_DIR})
add_executable(qtgo GameStruct.cpp GoTable.cpp main.cpp mainwindow.cpp ${HEADER_MOC} ${UIS_HEADERS})
target_link_libraries(qtgo Qt5::Widgets Qt5::Svg board engine utils sgf)
