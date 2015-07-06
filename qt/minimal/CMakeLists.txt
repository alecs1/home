project(Main)
cmake_minimum_required(VERSION 2.8)
cmake_policy(VERSION 3.2)

#cmake -DCMAKE_TOOLCHAIN_FILE=~/github/home/qt-android-cmake/toolchain/android.toolchain.cmake ../minimal
#run cmake
#run make

#region initialisation
if (${CMAKE_COMPILER_IS_GNUCXX})
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -std=c++11")
    set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g -O0 -fno-omit-frame-pointer -Wall -std=c++11")
    set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}  -D_GLIBCXX_DEBUG -fstack-protector-all -fstack-check")
    #if (NOT DEFINED ANDROID_ABI)
        set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fsanitize=address,bounds")
        #set (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -fsanitize=address -static-libasan")
    #endif (NOT DEFINED ANDROID_ABI)
endif (${CMAKE_COMPILER_IS_GNUCXX})

if ($CMAKE_COMPILER_IS_GNUCC})
    set (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -O0 -fno-omit-frame-pointer -Wall")
    set (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -fstack-protector-all -fstack-check")
    #if (NOT DEFINED ANDROID_ABI)
        set (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -fsanitize=address,bounds -static-libasan")
    #endif (NOT DEFINED ANDROID_ABI)
    set (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_RELEASE} -Wall")
endif ($CMAKE_COMPILER_IS_GNUCC})

if (ANDROID)
  set (QT_DIR "/home/alex/exec/Qt3/5.5/android_armv7")
  set (CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${QT_DIR}/lib/cmake/Qt5Core")
  set (CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${QT_DIR}/lib/cmake/Qt5Gui")
  set (CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${QT_DIR}/lib/cmake/Qt5Widgets")
  set (CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${QT_DIR}/lib/cmake/Qt5Bluetooth")
  set (ENV{QMAKESPEC} ${QT_DIR}/mkspecs/android-g++)
  set (ANDROID_TEMPLATE ${QT_DIR}/src/android/java)
  set (ANDROID_SDK "/home/alex/programs/adt-bundle-linux-x86_64-20140702/sdk")
  set (ENV{ANDROID_SDK} ${ANDROID_SDK})
  set (ENV{ANDROID_SDK_ROOT} ${ANDROID_SDK})
  set (ANDROID_NDK_INCLUDES ${ANDROID_NDK}/platforms/android-${ANDROID_NATIVE_API_LEVEL}/arch-${ANDROID_ARCH_NAME}/usr/include/android)
endif()


find_package(Qt5Widgets REQUIRED)
find_package(Qt5Bluetooth REQUIRED)


include_directories(${Qt5Widgets_INCLUDE_DIRS})

qt5_wrap_cpp(HEADERS_MOC Main.h)

if (ANDROID)
  add_library(Main SHARED Main.cpp ${HEADERS_MOC})
else ()
  add_executable(Main Main.cpp ${HEADERS_MOC})
endif()

target_link_libraries(Main Qt5::Widgets Qt5::Bluetooth)

if (ANDROID)
    set(PACKAGE_SOURCES "${CMAKE_CURRENT_LIST_DIR}/android")
    set(QT_ANDROID_ANT "/usr/bin/ant")
    include("~/github/home/qt-android-cmake/AddQtAndroidApk.cmake")
    add_qt_android_apk(Main_apk Main
        NAME "Main"
        PACKAGE_NAME "org.alecs1.Main"
        PACKAGE_SOURCES "${CMAKE_CURRENT_LIST_DIR}/android"
    )
endif (ANDROID)


