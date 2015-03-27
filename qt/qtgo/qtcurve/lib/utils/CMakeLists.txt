message("inside qtcurve-utils CMAKE_INCLUDE_PATH=" ${CMAKE_INCLUDE_PATH})
set(qtcurve_utils_SRCS
  color.cpp
  dirs.cpp
  log.cpp
  utils.cpp
  strs.cpp
  shadow.cpp
  timer.cpp
  options.cpp
  fd_utils.cpp
  process.cpp
  # DO NOT condition on QTC_ENABLE_X11 !!!
  # These provides dummy API functions so that x and non-x version are abi
  # compatible. There's no X11 linkage when QTC_ENABLE_X11 is off even though
  # these files are compiled
  x11utils.cpp
  x11helpers.cpp
  x11wrap.cpp)

if (NOT DEFINED ANDROID_ABI)
    set(qtcurve_utils_LINKS ${LIBEXECINFO_LIBRARIES} pthread)
endif()

if(NOT APPLE)
    if (NOT DEFINED ANDROID_ABI)
        set(qtcurve_utils_LINKS ${qtcurve_utils_LINKS} m rt dl)
    else (NOT DEFINED ANDROID_ABI)
        set(qtcurve_utils_LINKS ${qtcurve_utils_LINKS} m dl)
    endif()
endif()

include_directories("${CMAKE_CURRENT_BINARY_DIR}")

if(QTC_ENABLE_X11)
  set(qtcurve_utils_LINKS ${qtcurve_utils_LINKS} ${QTC_X11_LIBRARIES})
endif()

add_definitions("-DQTC_UTILS_INTERNAL -pthread")

if (NOT DEFINED ANDROID_ABI)
    add_library(qtcurve-utils SHARED ${qtcurve_utils_SRCS}
      "${CMAKE_BINARY_DIR}/version.cpp")
else (NOT DEFINED ANDROID_ABI)
    add_library(qtcurve-utils STATIC ${qtcurve_utils_SRCS}
      "${CMAKE_BINARY_DIR}/version.cpp")
endif()
set_source_files_properties("${CMAKE_BINARY_DIR}/version.cpp"
  PROPERTIES GENERATED 1)

add_dependencies(qtcurve-utils gen-git-ver)

target_link_libraries(qtcurve-utils ${qtcurve_utils_LINKS})

set_target_properties(qtcurve-utils PROPERTIES
  VERSION 2.0
  SOVERSION 2
  COMPILE_FLAGS "-fvisibility=hidden"
  LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  OUTPUT_NAME "qtcurve-utils${QTC_UTILSLIB_INFIX}")
if (NOT DEFINED ANDROID_ABI)
    install(TARGETS qtcurve-utils LIBRARY
      DESTINATION "${LIB_INSTALL_DIR}")
endif()
