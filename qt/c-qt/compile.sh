set -e

echo "Compiling qt_exports.o"
g++ -v -O0 -g -c -Wall -fpic qt_exports.cpp -o qt_exports.o  -I/usr/include/qt4/QtCore -I/usr/include/qt4/QtGui -I.  -I/usr/include/qt4  -DQT_WEBKIT -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARE

echo "Compiling qt_exports.so"
g++ -v -O0 -g -shared -o libqt_exports.so qt_exports.o -lQtGui -lQtCore -lpthread


# export LDFLAGS=-rpath=/home/alex/github/qt/c-qt
echo "Compiling c-example"
gcc -v -O0 -std=c99 -g -Wall c-example.c -o c-example -I. -L. -lqt_exports -Xlinker -rpath=/home/alex/github/qt/c-qt -lQtGui -lQtCore -lpthread -DQT_WEBKIT -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARE


