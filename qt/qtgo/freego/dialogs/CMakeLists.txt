project (FreeGo)

qt5_wrap_ui(UI_HEADERS_DIALOGS
    AddressDialog.ui
    GameChooser.ui)
#this is here because ui_<file>.h generated files are not found by the compilation process
include_directories(${CMAKE_CURRENT_BINARY_DIR})

qt5_wrap_cpp(MOC_HEADERS
    AdressDialog.h
    GameChooser.h)


add_library(dialogs
    AddressDialog.cpp
    GameChooser.cpp
    ${UI_HEADERS_DIALOGS}
    ${MOC_HEADERS} )
