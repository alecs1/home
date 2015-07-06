project(FreeGo)
cmake_minimum_required(VERSION 2.8)

#cmake -DCMAKE_TOOLCHAIN_FILE=~/github/home/qt-android-cmake/toolchain/android.toolchain.cmake ../qtgo
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
  set (QT_DIR "/home/alex/exec/Qt3/5.5/android_arm7")
  set (ANDROID_SDK "/home/alex/programs/adt-bundle-linux-x86_64-20140702/sdk")
  set (ENV{ANDROID_SDK} ${ANDROID_SDK})
  set (ENV{ANDROID_SDK_ROOT} ${ANDROID_SDK})
  set (ANDROID_NDK_INCLUDES ${ANDROID_NDK}/platforms/android-${ANDROID_NATIVE_API_LEVEL}/arch-${ANDROID_ARCH_NAME}/usr/include/android)
endif()



find_package(Qt5Widgets REQUIRED)
find_package(Qt5Bluetooth REQUIRED)

qt5_wrap_cpp(Main.h)

if (ANDROID)
  add_library(Main SHARED Main.cpp)
else ()
  add_executable(Main Main.cpp)
endif()

target_link_libraries(Main Qt5::Widgets, Qt5::Bluetooth)





