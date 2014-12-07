#include "mainwindow.h"
#include <QApplication>

#include "GameStruct.h"

GameStruct game;

int main(int argc, char *argv[])
{
    game.size = 19;

    QApplication a(argc, argv);
    MainWindow w;
    w.show();


    return a.exec();
}
