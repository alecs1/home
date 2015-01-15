# File generated by kdevelop's qmake manager.
# -------------------------------------------
# Subdir relative project main directory: ./src
# Target is an application:

FORMS += MCWorld/ui/maidens.ui \
         PuzzleWorld/ui/puzzle.ui
HEADERS += Coordinate.h \
           Ellipse.h \
           geometry.h \
           MainWindow.h \
           PhysicalObject.h \
           Pixmap.h \
           Rect.h \
           SinglePoint.h \
           Speed.h \
           Universe.h \
           UniverseWidget.h \
           Vector.h \
           World.h \
           MCWorld/MCWorld.h \
           MCWorld/ui/MaidensUi.h \
           MCWorld/ui/ui_maidens.h \
           BeesWorld/Bee.h \
           BeesWorld/BeesWorld.h \
           MainDock.h \
           ListerInterface.h \
           WorldBuddy.h \
           WorldControlInterface.h \
           WorldEmisar.h \
           WorldEmisarWidget.h \
           WIBuddyWidget.h \
           BeesWorld/ChainedBeesWorld.h \
           BeesWorld/SpectacularBeesWorld.h \
           PuzzleWorld/PuzzleWorld.h \
           PuzzleWorld/ui/PuzzleUi.h \
           PuzzleWorld/PuzzleWorldShow.h \
           PuzzleWorld/ui/ui_puzzle.h \
           BeesWorld/SimpleBeesWorld.h \
           ColisionsWorld/RectWorld.h \
           ColisionsWorld/CWRect.h \
           SpaceFightWorld/SpaceFightWorld.h \
 UniverseWidgetGL.h \
 UniverseWidgetBase.h \
 UniverseShowerGL.h \
 Global.h
SOURCES += coliziuni.cpp \
           Ellipse.cpp \
           geometry.cpp \
           MainWindow.cpp \
           PhysicalObject.cpp \
           Pixmap.cpp \
           Rect.cpp \
           SinglePoint.cpp \
           Speed.cpp \
           Universe.cpp \
           UniverseWidget.cpp \
           Vector.cpp \
           World.cpp \
           MCWorld/MCWorld.cpp \
           MCWorld/ui/MaidensUi.cpp \
           BeesWorld/Bee.cpp \
           BeesWorld/BeesWorld.cpp \
           MainDock.cpp \
           WorldBuddy.cpp \
           WorldEmisar.cpp \
           WorldEmisarWidget.cpp \
           WIBuddyWidget.cpp \
           BeesWorld/ChainedBeesWorld.cpp \
           BeesWorld/SpectacularBeesWorld.cpp \
           PuzzleWorld/PuzzleWorld.cpp \
           PuzzleWorld/ui/PuzzleUi.cpp \
           PuzzleWorld/PuzzleWorldShow.cpp \
           BeesWorld/SimpleBeesWorld.cpp \
           ColisionsWorld/RectWorld.cpp \
           ColisionsWorld/CWRect.cpp \
           SpaceFightWorld/SpaceFightWorld.cpp \
 UniverseWidgetGL.cpp \
 UniverseWidgetBase.cpp \
 UniverseShowerGL.cpp
RESOURCES = src.qrc
INCLUDEPATH += ../src
CONFIG += warn_on \
 qt \
 debug \
 debug_and_release
TEMPLATE = app

IDL_OPTIONS += -pg

CONFIG -= release

TARGET = ../bin/src


QT += widgets opengl

OTHER_FILES += \
    images/BeesWorld/bee-nt.png \
    images/BeesWorld/bee.png \
    images/BeesWorld/simple-bee.png \
    images/icons/folder_crystal.png \
    images/icons/parallel worlds.png \
    images/icons/parallel worlds.svg \
    images/MCWorld/boat.png \
    images/MCWorld/italian-notransparency.png \
    images/MCWorld/italian.png \
    images/MCWorld/maiden-notransparency.png \
    images/MCWorld/maiden.png \
    images/MCWorld/river.png \
    images/PuzzleWorld/tux-small.png \
    images/PuzzleWorld/tux.png \
    images/PuzzleWorld/yousmile-large.png \
    images/PuzzleWorld/yousmile.png

