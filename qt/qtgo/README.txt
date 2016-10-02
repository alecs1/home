Approximate steps for a full build:
cmake -DCMAKE_TOOLCHAIN_FILE=~/github/home/android-cmake/android.toolchain.cmake -DCMAKE_BUILD_TYPE=Debug -DANDROID_ABI="armeabi-v7a with NEON" ../qtgo
make
../qtgo/copy-pc-executables.sh
make
