project(FreeGo)
cmake_minimum_required(VERSION 2.8)

message(${CMAKE_SYSTEM_NAME})

#steps:
#do export QMAKESPEC="/home/alex/exec/Qt/5.4/android_armv7/mkspecs/android-g++"
#run cmake
#run make
#run copy-pc-executables.sh
#run make


#TODO - definition that we want, should work with CMake 3.1.x:
#if (CMAKE_SYSTEM_NAME STREQUAL Android)
#    message("Doing Android build")
#    add_custom_target(FeeGo_Android depends gnugo)
#else (CMAKE_SYSTEM_NAME STREQUAL Android)
#    message("Doing desktop build")
#    add_subdirectory("qtgo")
#endif (CMAKE_SYSTEM_NAME STREQUAL Android)

# TODO - definition that works with CMake 3.0.2
# This works because our command line is:
# cmake -DCMAKE_TOOLCHAIN_FILE=~/github/home/android-cmake/android.toolchain.cmake -DCMAKE_BUILD_TYPE=Debug -DANDROID_ABI="armeabi-v7a with NEON" ../qtgo

#add_library(bogus AndroidSpecificSetup.cmake)

if (${CMAKE_COMPILER_IS_GNUCXX})
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -O0 -fno-omit-frame-pointer -Wall -std=c++11")
        set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}  -D_GLIBCXX_DEBUG -fstack-protector-all -fstack-check")
    if (NOT DEFINED ANDROID_ABI)
        set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=address")
        set (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -fsanitize=address -static-libasan")
    endif (NOT DEFINED ANDROID_ABI)
endif (${CMAKE_COMPILER_IS_GNUCXX})

if ($CMAKE_COMPILER_IS_GNUCC})
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -O0 -fno-omit-frame-pointer -Wall")
        set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fstack-protector-all -fstack-check")
    if (NOT DEFINED ANDROID_ABI)
        set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fsanitize=address -static-libasan")
    endif (NOT DEFINED ANDROID_ABI)
endif ($CMAKE_COMPILER_IS_GNUCC})

if (DEFINED ANDROID_ABI)
    message("Doing Android build")
    file(COPY AndroidSpecificSetup.cmake DESTINATION .)
    set(QT_DIR "/home/alex/exec/Qt/5.4/android_armv7")
    set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${QT_DIR}/lib/cmake/Qt5Core")
    set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${QT_DIR}/lib/cmake/Qt5Gui")
    set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${QT_DIR}/lib/cmake/Qt5Widgets")
    set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${QT_DIR}/lib/cmake/Qt5Svg")
    set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${QT_DIR}/lib/cmake/Qt5DBus")
    message("CMAKE_MODULE_PATH=" ${CMAKE_MODULE_PATH})
    #set(QT_DIR "/home/alex/exec/Qt/5.3/android_armv7/")
    set(ENV{QMAKESPEC} ${QT_DIR}/mkspecs/android-g++)
    set(ANDROID_TEMPLATE ${QT_DIR}/src/android/java)
    set(BUILD_APK_TARGET_DIR "android-apk")
    set(ENV{BUILD_APK_TARGET_DIR} ${BUILD_APK_TARGET_DIR})
    set(ANDROID_SDK "/home/alex/programs/adt-bundle-linux-x86_64-20140702/sdk")
    set(ENV{ANDROID_SDK} ${ANDROID_SDK})
    set(ENV{ANDROID_SDK_ROOT} ${ANDROID_SDK})

    #TODO - this is also a sorry hack
    set (ANDROID_NDK_INCLUDES ${ANDROID_NDK}/platforms/android-${ANDROID_NATIVE_API_LEVEL}/arch-${ANDROID_ARCH_NAME}/usr/include/android)
    set (ANDROID_NDK_UNIX_INCLUDES ${ANDROID_NDK}/platforms/android-${ANDROID_NATIVE_API_LEVEL}/arch-${ANDROID_ARCH_NAME}/usr/include/sys)
    message("INCLUDE_DIRECTORIES=" ${INCLUDE_DIRECTORIES})
    #set (CMAKE_SYSROOT ${ANDROID_NDK}/platforms/android-${ANDROID_NATIVE_API_LEVEL}/arch-${ANDROID_ARCH_NAME})
    include_directories(SYSTEM ${ANDROID_NDK_UNIX_INCLUDES})
    message("INCLUDE_DIRECTORIES=" ${INCLUDE_DIRECTORIES})
    message("CMAKE_SYSROOT=" ${CMAKE_SYSROOT})

    #add_subdirectory("qtcurve")
    add_custom_target(AndroidSpecificSetup
                    ALL
                    COMMAND ${CMAKE_COMMAND} -D SOURCE=${CMAKE_CURRENT_SOURCE_DIR} -D BUILD_APK_TARGET_DIR=${BUILD_APK_TARGET_DIR} -D ANDROID_TEMPLATE=${ANDROID_TEMPLATE} -P AndroidSpecificSetup.cmake
                    COMMENT "Setting up structure for qmake system" VERBATIM)
    add_custom_target(FreeGo_Android
                    ALL
                    WORKING_DIRECTORY FreeGo
                    COMMAND ${QT_DIR}/bin/qmake -r INCLUDEPATH+=${ANDROID_NDK_INCLUDES} INCLUDE_PATH+=${ANDROID_NDK_UNIX_INCLUDES}
                    COMMAND make
                    COMMAND make install INSTALL_ROOT=${BUILD_APK_TARGET_DIR}
                    COMMAND ${ANDROID_SDK}/tools/android update project --path ${BUILD_APK_TARGET_DIR} --target android-10 --name FreeGo
                    #replace this androiddeployqt command with the private command to generate a release build uploadable to Google Play
                    #COMMAND ${QT_DIR}/bin/androiddeployqt --output ${BUILD_APK_TARGET_DIR} --verbose
                    #TODO - do some security here
                    COMMAND ${QT_DIR}/bin/androiddeployqt --output ${BUILD_APK_TARGET_DIR} --verbose --release --sign release.keystore androidrelease --storepass androidfebruarie --keypass androidfebruarie
                    DEPENDS gnugo AndroidSpecificSetup #qtcurve
                    COMMENT "Running qmake outside of CMake system" VERBATIM)
    #add_dependencies(FreeGo_Android qtcurve-qt5_static)
else (DEFINED ANDROID_ABI)
    message("Doing desktop build")
    add_subdirectory("qtgo")
    if (NOT WIN32)
        add_subdirectory("qtcurve")
    endif (NOT WIN32)
endif (DEFINED ANDROID_ABI)

add_subdirectory("gnugo")

message("Disable qtcurve from the build if it fails, it's optional")
message("ANDROID_SDK=" ${ANDROID_SDK})
message("CMAKE_PREFIX_PATH=" ${CMAKE_PREFIX_PATH})
message("CMAKE_SYSROOT=" ${CMAKE_SYSROOT})
