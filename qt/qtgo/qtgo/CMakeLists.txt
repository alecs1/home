#nice documentation: https://qt-project.org/quarterly/view/using_cmake_to_build_qt_projects

project(FreeGo)
cmake_minimum_required(VERSION 2.8)

set (CMAKE_VERBOSE_MAKEFILE true)


find_package(Qt5Widgets REQUIRED)
find_package(Qt5Svg REQUIRED)
find_package(Qt5Bluetooth REQUIRED)

if (NOT ANDROID)
    add_definitions(-DWithQt5Quick)
    find_package(Qt5Quick REQUIRED)
    find_package(Qt5QuickWidgets REQUIRED)
endif (NOT ANDROID)


find_package(Qt5Qml REQUIRED) #required for Qt5Multimedia; would freeze at startup on certain phones if missing.
find_package(Qt5Multimedia REQUIRED)
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
qt5_wrap_ui(UI_HEADERS mainwindow.ui GameSettings.ui PlayerWidget.ui GameEndDialog.ui ConfirmMoveDialog.ui
    RoundInfo.ui Handicap.ui HandicapDialog.ui BusyDialog.ui SettingsWidget.ui SettingsDialog.ui)


#how to debug a CMake script:
message("UI_HEADERS:" ${UI_HEADERS})


#qt moc
qt5_wrap_cpp(HEADER_MOC
    mainwindow.h
    DrawAreaWidget.h
    GoTable.h
    GameSettings.h
    MiniGameSettings.h
    PlayerWidget.h
    GameEndDialog.h
    ConfirmMoveDialog.h
    RoundInfo.h
    Handicap.h
    HandicapDialog.h
    BusyDialog.h
    AboutDialog.h
    HelpDialog.h
    SettingsWidget.h
    SettingsDialog.h
    DebugStuff.h)

qt5_add_resources(RESOURCES_RCC res.qrc)


include_directories(${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR}/btchat)

set (ALL_SOURCES mainwindow.cpp GameStruct.cpp DrawAreaWidget.cpp GoTable.cpp main.cpp GameSettings.cpp MiniGameSettings.cpp PlayerWidget.cpp
    GameEndDialog.cpp ConfirmMoveDialog.cpp RoundInfo.cpp SaveFile.cpp Global.cpp AboutDialog.cpp
    HelpDialog.cpp Handicap.cpp HandicapDialog.cpp BusyDialog.cpp ThirdPartyInfo.cpp SettingsWidget.cpp
    SettingsDialog.cpp Utils.cpp Settings.cpp DebugStuff.cpp
    ${HEADER_MOC} ${UI_HEADERS} ${RESOURCES_RCC})

if (ANDROID)
    add_library(FreeGo SHARED ${ALL_SOURCES})
else ()
    add_executable(FreeGo ${ALL_SOURCES})
endif()

target_link_libraries(FreeGo Qt5::Widgets Qt5::Svg Qt5::Multimedia Qt5::Bluetooth board engine utils sgf patterns btchat network)

if (ANDROID)
    target_link_libraries(FreeGo log)
else ()
    target_link_libraries(FreeGo Qt5::Quick Qt5::QuickWidgets)
endif (ANDROID)


add_subdirectory("btchat")
add_subdirectory("network")

add_custom_target(DocNetwork Notes.txt)

