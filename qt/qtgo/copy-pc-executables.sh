# copy executables that need to run on this pc from the x86 version of the build

#From Linux
cp ../qtgo-build/gnugo/patterns/compress_fuseki gnugo/patterns/
cp ../qtgo-build/gnugo/patterns/extract_fuseki  gnugo/patterns/
cp ../qtgo-build/gnugo/patterns/joseki      gnugo/patterns/
cp ../qtgo-build/gnugo/patterns/mkeyes      gnugo/patterns/
cp ../qtgo-build/gnugo/patterns/mkmcpat     gnugo/patterns/
cp ../qtgo-build/gnugo/patterns/mkpat       gnugo/patterns/
cp ../qtgo-build/gnugo/patterns/uncompress_fuseki   gnugo/patterns/

#Windows
cp ../qtgo-build/gnugo/patterns/Debug/compress_fuseki.exe gnugo/patterns/
cp ../qtgo-build/gnugo/patterns/Debug/extract_fuseki.exe  gnugo/patterns/
cp ../qtgo-build/gnugo/patterns/Debug/joseki.exe      gnugo/patterns/
cp ../qtgo-build/gnugo/patterns/Debug/mkeyes.exe      gnugo/patterns/
cp ../qtgo-build/gnugo/patterns/Debug/mkmcpat.exe     gnugo/patterns/
cp ../qtgo-build/gnugo/patterns/Debug/mkpat.exe       gnugo/patterns/
cp ../qtgo-build/gnugo/patterns/Debug/uncompress_fuseki.exe   gnugo/patterns/


cp ../qtgo-build/qtcurve/tools/qt-dump-png qtcurve/tools/

