project(FreeGo)
cmake_minimum_required(VERSION 2.8)

#steps:
#Android:
#cmake -DCMAKE_TOOLCHAIN_FILE=/usr/share/ECM/toolchain/Android.cmake ../qtgo -DCMAKE_BUILD_TYPE=Debug -DCMAKE_PREFIX_PATH=/home/alex/exec/Qt/5.9.3/android_armv7 -DQTANDROID_EXPORTED_TARGET=FreeGo -DANDROID_APK_DIR=../qtgo/freego/android
#make create-apk-FreeGo



# After building the application, you will need to generate an APK that can be
# deployed to an Android device. This module integrates androiddeployqt support
# to help with this for Qt-based projects. To enable this, set the
# ``QTANDROID_EXPORTED_TARGET`` variable to the target you wish to export as an
# APK, as well as ``ANDROID_APK_DIR`` to a directory containing some basic
# information. This will create a ``create-apk-<target>`` target that will
# generate the APK file.  See the `Qt on Android deployment documentation
# <http://doc.qt.io/qt-5/deployment-android.html>`_ for more information.
#
# For example, you could do::
#
#   cmake \
#     -DCMAKE_TOOLCHAIN_FILE=/usr/share/ECM/toolchain/AndroidToolchain.cmake \
#     -DQTANDROID_EXPORTED_TARGET=myapp \
#     -DANDROID_APK_DIR=myapp-apk
#   make
#   make create-apk-myapp
#
# The APK would then be found in ``myapp_build_apk/bin`` in the build directory.
#
# The create-apk-myapp target will be able to take an ARGS parameter with further
# arguments for androiddeployqt. For example, one can use::
#
#   make create-apk-myapp ARGS="--install"
#
# To install the apk to test. To generate a signed apk, one can do it with the
# following syntax::
#
#   make create-apk-myapp ARGS="--sign ~/my.keystore alias_name"
#
# See Android documentation on how to create a keystore to use


#Windows:
#cmake -G "Visual Studio 12 2013 Win64" ../qtgo
#run cmake
#run make
#run copy-pc-executables.sh
#run make

if (${CMAKE_COMPILER_IS_GNUCXX})
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -std=c++11 -fPIC")
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -fPIC")
    set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g -O0 -fno-omit-frame-pointer -Wall -std=c++11")
    set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}  -D_GLIBCXX_DEBUG -fstack-protector-all -fstack-check")
    if (NOT DEFINED ANDROID)
        #set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fsanitize=address,bounds")
        #set (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -fsanitize=address")
    endif (NOT DEFINED ANDROID)
endif (${CMAKE_COMPILER_IS_GNUCXX})

if ($CMAKE_COMPILER_IS_GNUCC})
    set (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -O0 -fno-omit-frame-pointer -Wall")
    set (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -fstack-protector-all -fstack-check")
    if (NOT DEFINED ANDROID)
        #set (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -fsanitize=address,bounds -static-libasan")
        #set (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -fsanitize=address")
    endif (NOT DEFINED ANDROID)
    set (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_RELEASE} -Wall")
endif ($CMAKE_COMPILER_IS_GNUCC})


if ( WIN32 OR ANDROID )
    if (ANDROID)
        set(QT_DIR "/home/alex/exec/Qt/5.9.3/android_armv7")
#         set(QT_DIR "../Qt/5.9.3/android_armv7")
        #set(QT_DIR "C:/Qt/5.5/android_armv7")
    else (ANDROID)
        set(QT_DIR "C:/Qt/5.5/msvc2013_64")
    endif (ANDROID)
    set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${QT_DIR}")
    set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${QT_DIR}/lib/cmake/Qt5Core")
    set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${QT_DIR}/lib/cmake/Qt5Gui")
    set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${QT_DIR}/lib/cmake/Qt5Widgets")
    set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${QT_DIR}/lib/cmake/Qt5Svg")
    set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${QT_DIR}/lib/cmake/Qt5DBus")
    set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${QT_DIR}/lib/cmake/Qt5Bluetooth")
    set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${QT_DIR}/lib/cmake/Qt5Multimedia")
    set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${QT_DIR}/lib/cmake/Qt5Quick")
    set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${QT_DIR}/lib/cmake/Qt5QuickWidgets")
    set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${QT_DIR}/lib/cmake/Qt5Qml")
    message("CMAKE_PREFIX_PATH=" ${CMAKE_PREFIX_PATH})
    set(CMAKE_MODULE_PATH ${CMAKE_PREFIX_PATH})
endif (WIN32 OR ANDROID)

message("QT_DIR:${QT_DIR}")
message("CMAKE_SYSTEM_PREFIX_PATH:${CMAKE_SYSTEM_PREFIX_PATH}")
foreach(path ${CMAKE_PREFIX_PATH})
  message("Path:${path}")
endforeach(path)
foreach(path ${CMAKE_MODULE_PATH})
  message("Path:${path}")
endforeach(path)

if (ANDROID)
    #file(COPY AndroidSpecificSetup.cmake DESTINATION .)
    #message("CMAKE_MODULE_PATH=" ${CMAKE_MODULE_PATH})
    set(ENV{QMAKESPEC} ${QT_DIR}/mkspecs/android-g++)
    set(ANDROID_TEMPLATE ${QT_DIR}/src/android/java)
    #set(BUILD_APK_TARGET_DIR "android-apk")
    #set(ENV{BUILD_APK_TARGET_DIR} ${BUILD_APK_TARGET_DIR})
    set(ANDROID_SDK "/home/alex/exec/android-sdk-linux")
    #set(ANDROID_SDK "/usr/lib/android-sdk")
    #set(ANDROID_SDK "C:/android/sdk")
    set(ENV{ANDROID_SDK} ${ANDROID_SDK})
    #set(ENV{ANDROID_SDK_ROOT} ${ANDROID_SDK})
    #set(ENV{ANDROID_NATIVE_API_LEVEL} 15)
    #set(__ANDROID_API__ 15)
    set (ANDROID_NDK_INCLUDES ${ANDROID_NDK}/sysroot/usr/include)
    #for ndk unix is for qt-curve
    set (ANDROID_NDK_UNIX_INCLUDES ${ANDROID_NDK}/sysroot/usr/include/sys)
    set (ANDROID_NDK_ARCH_INCLUDES ${ANDROID_NDK}/sysroot/usr/include/${ANDROID_TOOLCHAIN})
    include_directories(SYSTEM ${QT_DIR} ${ANDROID_NDK_INCLUDES} ${ANDROID_NDK_UNIX_INCLUDES} ${ANDROID_NDK_ARCH_INCLUDES} ".")
    message("ANDROID_NDK_ARCH_INCLUDES=" ${ANDROID_NDK_ARCH_INCLUDES})
endif(ANDROID)

add_subdirectory("freego")

#qtcurve is not used for now and only slows down Android build
if ( (NOT WIN32) AND (NOT ANDROID) )
    #add_subdirectory("qtcurve")
endif ( (NOT WIN32) AND (NOT ANDROID) )


add_subdirectory("gnugo")
# 
# if (ANDROID)
#     set(PACKAGE_SOURCES "${CMAKE_CURRENT_LIST_DIR}/freego/android")
#     set(QT_ANDROID_ANT "/usr/bin/ant")
#     include("../../qt-android-cmake/AddQtAndroidApk.cmake")
#     add_qt_android_apk(FreeGo_apk FreeGo
#         NAME "FreeGo"
#         PACKAGE_NAME "org.alecs1.FreeGo"
#         PACKAGE_SOURCES "${CMAKE_CURRENT_LIST_DIR}/freego/android"
#         KEYSTORE ${CMAKE_CURRENT_LIST_DIR}/freego/android/release.keystore androidrelease
#         #BUILDTOOLS_REVISION "27.0.0"
#     )
# endif (ANDROID)

message("Disable qtcurve from the build if it fails, it's optional")
