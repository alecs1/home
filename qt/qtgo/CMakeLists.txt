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

if (DEFINED ANDROID_ABI)
    message("Doing Android build")
    file(COPY AndroidSpecificSetup.cmake DESTINATION .)
    set(QT_DIR "/home/alex/exec/Qt/5.4/android_armv7/")
    set(ENV{QMAKESPEC} ${QT_DIR}/mkspecs/android-g++)
    set(ANDROID_TEMPLATE ${QT_DIR}/src/android/java)
    set(BUILD_APK_TARGET_DIR "android-apk")
    set(ENV{BUILD_APK_TARGET_DIR} ${BUILD_APK_TARGET_DIR})
    set(ANDROID_SDK "/home/alex/programs/adt-bundle-linux-x86_64-20140702/sdk")
    set(ENV{ANDROID_SDK} ${ANDROID_SDK})
    set(ENV{ANDROID_SDK_ROOT} ${ANDROID_SDK})

    #TODO - this is also a sorry hack
    set (ANDROID_NDK_INCLUDES ${ANDROID_NDK}/platforms/android-${ANDROID_NATIVE_API_LEVEL}/arch-${ANDROID_ARCH_NAME}/usr/include/android)

    add_custom_target(AndroidSpecificSetup
                    ALL
                    COMMAND ${CMAKE_COMMAND} -D SOURCE=${CMAKE_CURRENT_SOURCE_DIR} -D BUILD_APK_TARGET_DIR=${BUILD_APK_TARGET_DIR} -D ANDROID_TEMPLATE=${ANDROID_TEMPLATE} -P AndroidSpecificSetup.cmake
                    COMMENT "Setting up structure for qmake system" VERBATIM)
    add_custom_target(FreeGo_Android
                    ALL
                    WORKING_DIRECTORY FreeGo
                    COMMAND ${QT_DIR}/bin/qmake -r INCLUDEPATH+=${ANDROID_NDK_INCLUDES}
                    COMMAND make
                    COMMAND make install INSTALL_ROOT=${BUILD_APK_TARGET_DIR}
                    COMMAND ${ANDROID_SDK}/tools/android update project --path ${BUILD_APK_TARGET_DIR} --target android-10 --name FreeGo
                    COMMAND ${QT_DIR}/bin/androiddeployqt --output ${BUILD_APK_TARGET_DIR} --verbose
                    DEPENDS gnugo AndroidSpecificSetup
                    COMMENT "Running qmake outside of CMake system" VERBATIM)
else (DEFINED ANDROID_ABI)
    message("Doing desktop build")
    add_subdirectory("qtgo")
endif (DEFINED ANDROID_ABI)

add_subdirectory("gnugo")
