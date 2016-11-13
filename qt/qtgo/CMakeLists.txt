project(FreeGo)
cmake_minimum_required(VERSION 2.8)

message(${CMAKE_SYSTEM_NAME})

#steps:
#Android:
#cmake -DCMAKE_TOOLCHAIN_FILE=../../qt-android-cmake/toolchain/android.toolchain.cmake ../qtgo -DCMAKE_BUILD_TYPE=Debug
#Windows:
#cmake -G "Visual Studio 12 2013 Win64" ../qtgo
#run cmake
#run make
#run copy-pc-executables.sh
#run make

#force the newest gcc for now
#set (CMAKE_CXX_COMPILER "/usr/bin/g++-5")
#set (CMAKE_C_COMPILER "/usr/bin/gcc-5")

if (${CMAKE_COMPILER_IS_GNUCXX})
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -std=c++11 -fPIC")
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -fPIC")
    set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g -O0 -fno-omit-frame-pointer -Wall -std=c++11")
    set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}  -D_GLIBCXX_DEBUG -fstack-protector-all -fstack-check")
    if (NOT DEFINED ANDROID)
        set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fsanitize=address,bounds")
    endif (NOT DEFINED ANDROID)
endif (${CMAKE_COMPILER_IS_GNUCXX})

if ($CMAKE_COMPILER_IS_GNUCC})
    set (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -O0 -fno-omit-frame-pointer -Wall")
    set (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -fstack-protector-all -fstack-check")
    if (NOT DEFINED ANDROID)
        set (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -fsanitize=address,bounds -static-libasan")
    endif (NOT DEFINED ANDROID)
    set (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_RELEASE} -Wall")
endif ($CMAKE_COMPILER_IS_GNUCC})


if ( WIN32 OR ANDROID )
    if (ANDROID)
        set(QT_DIR "/home/alex/exec/Qt3/5.7/android_armv7")
        #set(QT_DIR "C:/Qt/5.5/android_armv7")
    else (ANDROID)
        set(QT_DIR "C:/Qt/5.5/msvc2013_64")
    endif (ANDROID)
    #set(QT_DIR "/home/alex/exec/Qt2/5.4/Src/qtbase")
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
endif (WIN32 OR ANDROID)

if (ANDROID)
    #file(COPY AndroidSpecificSetup.cmake DESTINATION .)
    message("CMAKE_MODULE_PATH=" ${CMAKE_MODULE_PATH})
    set(ENV{QMAKESPEC} ${QT_DIR}/mkspecs/android-g++)
    set(ANDROID_TEMPLATE ${QT_DIR}/src/android/java)
    #set(BUILD_APK_TARGET_DIR "android-apk")
    #set(ENV{BUILD_APK_TARGET_DIR} ${BUILD_APK_TARGET_DIR})
    set(ANDROID_SDK "/home/alex/exec/android-sdk-linux")
    #set(ANDROID_SDK "/usr/lib/android-sdk")
    #set(ANDROID_SDK "C:/android/sdk")
    set(ENV{ANDROID_SDK} ${ANDROID_SDK})
    set(ENV{ANDROID_SDK_ROOT} ${ANDROID_SDK})
    set (ANDROID_NDK_INCLUDES ${ANDROID_NDK}/platforms/android-${ANDROID_NATIVE_API_LEVEL}/arch-${ANDROID_ARCH_NAME}/usr/include/android)
    #for ndk unix is for qt-curve
    set (ANDROID_NDK_UNIX_INCLUDES ${ANDROID_NDK}/platforms/android-${ANDROID_NATIVE_API_LEVEL}/arch-${ANDROID_ARCH_NAME}/usr/include/sys)
    include_directories(SYSTEM ${ANDROID_NDK_INCLUDES} ${ANDROID_NDK_UNIX_INCLUDES})
endif(ANDROID)

add_subdirectory("qtgo")

#qtcurve is not used for now and only slows down Android build
if ( (NOT WIN32) AND (NOT ANDROID) )
    #add_subdirectory("qtcurve")
endif ( (NOT WIN32) AND (NOT ANDROID) )


add_subdirectory("gnugo")

if (ANDROID)
    set(PACKAGE_SOURCES "${CMAKE_CURRENT_LIST_DIR}/qtgo/android")
    set(QT_ANDROID_ANT "/usr/bin/ant")
    include("../../qt-android-cmake/AddQtAndroidApk.cmake")
    add_qt_android_apk(FreeGo_apk FreeGo
        NAME "FreeGo"
        PACKAGE_NAME "org.alecs1.FreeGo"
        PACKAGE_SOURCES "${CMAKE_CURRENT_LIST_DIR}/qtgo/android"
        KEYSTORE ${CMAKE_CURRENT_LIST_DIR}/qtgo/android/release.keystore androidrelease
    )
endif (ANDROID)

message("Disable qtcurve from the build if it fails, it's optional")
message("ANDROID_SDK=" ${ANDROID_SDK})
message("CMAKE_PREFIX_PATH=" ${CMAKE_PREFIX_PATH})
message("CMAKE_SYSROOT=" ${CMAKE_SYSROOT})
