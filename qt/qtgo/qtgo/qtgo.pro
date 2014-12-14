#-------------------------------------------------
#
# Project created by QtCreator 2014-12-06T21:54:40
#
#-------------------------------------------------

QT       += core gui svg

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = qtgo
TEMPLATE = app

QMAKE_CXXFLAGS += -std=c++11


SOURCES += main.cpp\
        mainwindow.cpp \
    GoTable.cpp \
    GameStruct.cpp

HEADERS  += mainwindow.h \
    GoTable.h \
    GameStruct.h

FORMS    += mainwindow.ui \
    GameSettings.ui

OTHER_FILES += \
    CMakeLists.txt
