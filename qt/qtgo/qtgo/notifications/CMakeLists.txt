project (FreeGo)

qt5_wrap_ui(UI_HEADERS_NOTIFICATIONS
    DockedNotif.ui)
#this is here because ui_<file>.h generated files are not found by the compilation process
include_directories(${CMAKE_CURRENT_BINARY_DIR})

qt5_wrap_cpp(MOC_HEADERS
    DockedNotif.h)


add_library(notifications
    DockedNotif.cpp
    ${UI_HEADERS_NOTIFICATIONS}
    ${MOC_HEADERS} )
