set(qtcurve_SRCS
  prepolish.cpp
  qtcurve.cpp
  qtcurve_api.cpp
  qtcurve_plugin.cpp
  qtcurve_primitive.cpp
  qtcurve_utils.cpp
  windowmanager.cpp
  blurhelper.cpp
  utils.cpp
  shortcuthandler.cpp
  argbhelper.cpp
  shadowhelper.cpp)
set(qtcurve_MOC_HDRS
  qtcurve.h
  qtcurve_p.h
  qtcurve_plugin.h
  windowmanager.h
  blurhelper.h
  shortcuthandler.h
  argbhelper.h
  shadowhelper.h)

translate_add_sources(${qtcurve_SRCS} ${qtcurve_MOC_HDRS})

if(NOT ENABLE_QT5)
  return()
endif()

if(QTC_QT5_ENABLE_QTQUICK2)
  add_definitions(-DQTC_QT5_ENABLE_QTQUICK2=1)
endif()

set(qtcurve_style_common_SRCS
  ../common/common.cpp
  ../common/config_file.cpp)

#TODO-hack: we're copying from the pc build, yet another abonimation
message("crap does not work, do the following by hand:")
message(cp "${CMAKE_SOURCE_DIR}/../qtgo-build/qtcurve/tools/qt-dump-png" "${PROJECT_BINARY_DIR}/tools/qt-dump-png")

if (DEFINED ANDROID_ABI)
    add_custom_target(copyFromPCBuild
                      COMMAND cp "${CMAKE_SOURCE_DIR}/../qtgo-build/qtcurve/tools/qt-dump-png" "${PROJECT_BINARY_DIR}/tools/qt-dump-png")
else (DEFINED ANDROID_ABI)
    add_custom_target(copyFromPCBuild)
endif ()

message("Will execute qt-dump-png")
foreach(pngname check_on check_x_on dialog_error dialog_information
    dialog_warning)
  message("qt-dump-png for ${pngname}:" "${PROJECT_BINARY_DIR}/tools/qt-dump-png"
    "${PROJECT_SOURCE_DIR}/data/${pngname}.png" "qtc_${pngname}"
    "${CMAKE_CURRENT_BINARY_DIR}/${pngname}-png.h")

  add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${pngname}-png.h"
    COMMAND "${PROJECT_BINARY_DIR}/tools/qt-dump-png"
    "${PROJECT_SOURCE_DIR}/data/${pngname}.png" "qtc_${pngname}"
    "${CMAKE_CURRENT_BINARY_DIR}/${pngname}-png.h"
    DEPENDS qt-dump-png copyFromPCBuild "${PROJECT_SOURCE_DIR}/data/${pngname}.png")
  add_custom_target(qtc_qt5_${pngname}_hdr
    DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${pngname}-png.h")
endforeach()

set(qtcurve_SRCS ${qtcurve_SRCS} ${qtcurve_style_common_SRCS})

add_definitions(-DQT_PLUGIN)
include_directories(
  "${CMAKE_CURRENT_SOURCE_DIR}"
  "${CMAKE_CURRENT_BINARY_DIR}"
  ${Qt5Widgets_PRIVATE_INCLUDE_DIRS})
if(NOT QTC_QT5_ENABLE_KDE)
  qt5_wrap_cpp(qtcurve_MOC_SRCS ${qtcurve_MOC_HDRS})
else()
  set(qtcurve_MOC_SRCS)
endif()


add_library(qtcurve-qt5 MODULE ${qtcurve_SRCS} ${qtcurve_MOC_SRCS})
set_target_properties(qtcurve-qt5 PROPERTIES
      OUTPUT_NAME "qtcurve"
      LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
      PREFIX "")


add_dependencies(qtcurve-qt5 qtc_qt5_check_on_hdr qtc_qt5_check_x_on_hdr
  qtc_qt5_dialog_error_hdr qtc_qt5_dialog_information_hdr
  qtc_qt5_dialog_warning_hdr)
  target_link_libraries(qtcurve-qt5 ${QTC_QT5_LINK_LIBS} qtcurve-utils)

if (DEFINED ANDROID_ABI)
    message("Doing Android build")
    add_library(qtcurve-qt5_static STATIC ${qtcurve_SRCS} ${qtcurve_MOC_SRCS})
    set_target_properties(qtcurve-qt5_static PROPERTIES
          OUTPUT_NAME "libqtcurve"
          LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
          PREFIX "")
    add_dependencies(qtcurve-qt5_static qtc_qt5_check_on_hdr qtc_qt5_check_x_on_hdr
      qtc_qt5_dialog_error_hdr qtc_qt5_dialog_information_hdr
      qtc_qt5_dialog_warning_hdr)
    target_link_libraries(qtcurve-qt5_static ${QTC_QT5_LINK_LIBS} qtcurve-utils)
endif()

#install(TARGETS qtcurve-qt5 LIBRARY DESTINATION ${QTCURVE_STYLE_DIR})

if(QTC_QT5_ENABLE_KDE)
  foreach(dir DATA_INSTALL_DIR KDE_INSTALL_DATADIR CMAKE_INSTALL_DATADIR
      SHARE_INSTALL_PREFIX KDE_INSTALL_DATAROOTDIR CMAKE_INSTALL_DATAROOTDIR)
    unset(${dir})
    unset(${dir} CACHE)
  endforeach()
  include(KDEInstallDirs)
  install(FILES qtcurve.themerc
    DESTINATION ${KDE_INSTALL_DATADIR}/kstyle/themes)
endif()
