#include "mainwindow.h"
#include <QApplication>

#include "GameStruct.h"

#include <QDir>

//GameStruct game;

int main(int argc, char *argv[])
{

    Q_INIT_RESOURCE(res);
    //game.size = 19;

    QApplication a(argc, argv);
    MainWindow w;
    w.show();


    return a.exec();
}
