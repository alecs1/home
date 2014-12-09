project(qtgo)
cmake_minimum_required(VERSION 2.8)


find_package(Qt5Widgets)
find_package(Qt5Svg)
include_directories(${Qt5Widgets_INCLUDE_DIRS})
include_directories(${Qt5Svg_INCLUDE_DIRS})


# Add compiler flags for building executables (-fPIE), required by Qt
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${Qt5Widgets_EXECUTABLE_COMPILE_FLAGS}")


#qt UI
qt5_wrap_ui(UIS_HEADERS mainwindow.ui GameSettings.ui)


#how to debug a CMake script:
message("UIS_HEADERS:" ${UIS_HEADERS})


#qt moc
qt5_wrap_cpp(HEADER_MOC GoTable.h mainwindow.h)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})
add_executable(qtgo GameStruct.cpp GoTable.cpp main.cpp mainwindow.cpp ${HEADER_MOC} ${UIS_HEADERS})
TARGET_LINK_LIBRARIES(qtgo Qt5::Widgets Qt5::Svg)
