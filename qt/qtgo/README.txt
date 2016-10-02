Linux
mkdir qtgo-build
cd qtgo-build
cmake ../qtgo


Android.
Building for Android requires having a build for the host computer first.

* first time only:
copy the private file qtgo/android/release.keystore


* run CMake in the new directory pointing to correct toolchain file
cmake -DCMAKE_TOOLCHAIN_FILE=../../android-cmake/android.toolchain.cmake -DCMAKE_BUILD_TYPE=Debug -DANDROID_ABI="armeabi-v7a with NEON" ../qtgo


* make


* Copy the generated executables from a PC build, since these need to run on the current PC, on on Arm
../qtgo/copy-pc-executables.sh

* make
input release.keystore password when requested
