Windows
"C:\Program Files (x86)\CMake\bin\cmake.exe" -G "Visual Studio 12 2013 Win64"
distr-ip.sln


Linux
cmake -G Makefile (or something like)
make


Android
Android is split into two separate build processes.
    client:
        cd GameSpecific
        make.bat debug
        cd ..
        adb install -r Package/bin/<AppName>.apk
        adb shell am start <AppName>/.MainActivity
    The client builds with our special Android entry point and libraries, and is a GUI application that runs the image processing code at start.


    server:
        C:\android-ndk32-r10-x64\ndk-build V=1  1>out.txt 2>&1
        cp obj/local/armeabi-v7a/distr-ip <some-path-on-you-phone>
    It builds with standard Android NDK and is a console application. The relevant files: jni/Android.mk and jni/Application.mk. Both link boost statically, and the server also has the problem of linking explicitly the STL.


