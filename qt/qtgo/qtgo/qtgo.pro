######################################################################
# Automatically generated by qmake (3.0) Sun Jan 11 14:48:22 2015
######################################################################

TEMPLATE = app
TARGET = FreeGo
INCLUDEPATH += .

#CONFIG += debug

QT += widgets svg

QMAKE_CXXFLAGS += -fstack-protector-all -fstack-check -D_GLIBCXX_DEBUG -g -O0 -Wall -std=c++11

INCLUDEPATH +=  "../gnugo_include/gnugo" \
                "../gnugo_include/gnugo/sgf" \
                "../gnugo_include/gnugo/utils"

LIBS += -L../gnugo/engine -L../gnugo/utils -L../gnugo/sgf -L ../gnugo/patterns -lboard -lengine -lutils -lsgf -lpatterns

# Input
HEADERS += GameSettings.h \
           GameStruct.h \
           Global.h \
           GoTable.h \
           mainwindow.h \
           PlayerWidget.h \
           GameEndDialog.h \
           ConfirmMoveDialog.h \
            ../gnugo/engine/gnugo.h \
    ../gnugo/engine/liberty.h \
    RoundInfo.h \
    SaveFile.h
FORMS += GameSettings.ui mainwindow.ui PlayerWidget.ui \
           GameEndDialog.ui \
           ConfirmMoveDialog.ui \
    RoundInfo.ui
SOURCES += GameSettings.cpp \
           GameStruct.cpp \
           GoTable.cpp \
           main.cpp \
           mainwindow.cpp \
           PlayerWidget.cpp \
           GameEndDialog.cpp \
    ConfirmMoveDialog.cpp \
    RoundInfo.cpp \
    SaveFile.cpp \
    Global.cpp

OTHER_FILES += AndroidManifest.xml \
    notes.txt \
    ../AndroidSpecificSetup.cmake \
    save-format.json

RESOURCES += \
    res.qrc

ANDROID_PACKAGE_SOURCE_DIR = $$PWD/android

