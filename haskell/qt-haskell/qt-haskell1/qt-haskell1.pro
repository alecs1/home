TEMPLATE = app
CONFIG += console
CONFIG -= app_bundle
CONFIG -= qt

SOURCES += \
    c_export.c

OTHER_FILES += \
    main.py \
    c_import.hs \
    README.txt

HEADERS += \
    c_export.h

