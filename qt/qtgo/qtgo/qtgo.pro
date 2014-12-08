#-------------------------------------------------
#
# Project created by QtCreator 2014-12-06T21:54:40
#
#-------------------------------------------------

QT       += core gui svg

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = qtgo
TEMPLATE = app


SOURCES += main.cpp\
        mainwindow.cpp \
    GoTable.cpp \
    GameStruct.cpp

HEADERS  += mainwindow.h \
    GoTable.h \
    GameStruct.h

FORMS    += mainwindow.ui \
    GameSetting.ui
