# copy executables that need to run on this pc from the x86 version of the build

#From Linux
cp ../qtgo-build/gnugo-lib/patterns/compress_fuseki gnugo-lib/patterns/
cp ../qtgo-build/gnugo-lib/patterns/extract_fuseki  gnugo-lib/patterns/
cp ../qtgo-build/gnugo-lib/patterns/joseki      gnugo-lib/patterns/
cp ../qtgo-build/gnugo-lib/patterns/mkeyes      gnugo-lib/patterns/
cp ../qtgo-build/gnugo-lib/patterns/mkmcpat     gnugo-lib/patterns/
cp ../qtgo-build/gnugo-lib/patterns/mkpat       gnugo-lib/patterns/
cp ../qtgo-build/gnugo-lib/patterns/uncompress_fuseki   gnugo-lib/patterns/

#Windows
cp ../qtgo-build/gnugo-lib/patterns/Debug/compress_fuseki.exe gnugo-lib/patterns/
cp ../qtgo-build/gnugo-lib/patterns/Debug/extract_fuseki.exe  gnugo-lib/patterns/
cp ../qtgo-build/gnugo-lib/patterns/Debug/joseki.exe      gnugo-lib/patterns/
cp ../qtgo-build/gnugo-lib/patterns/Debug/mkeyes.exe      gnugo-lib/patterns/
cp ../qtgo-build/gnugo-lib/patterns/Debug/mkmcpat.exe     gnugo-lib/patterns/
cp ../qtgo-build/gnugo-lib/patterns/Debug/mkpat.exe       gnugo-lib/patterns/
cp ../qtgo-build/gnugo-lib/patterns/Debug/uncompress_fuseki.exe   gnugo-lib/patterns/


cp ../qtgo-build/qtcurve/tools/qt-dump-png qtcurve/tools/

