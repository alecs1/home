#include "mainwindow.h"
#include <QApplication>

#include "GameStruct.h"

//#include <QDir>
#include <QPluginLoader>
#include <QStylePlugin>
#include <QStyle>

//GameStruct game;

int main(int argc, char *argv[])
{

    Q_INIT_RESOURCE(res);
    //game.size = 19;

    QApplication a(argc, argv);

    //dynamic loading on pc:
    QPluginLoader loader("qtcurve.so", &a);
    QObject* pluginInstance = loader.instance();
    if (pluginInstance != NULL) {
        printf("%s - loaded qtcurve.so\n", __func__);
        QStylePlugin* qtCurveStylePlugin = qobject_cast<QStylePlugin*>(pluginInstance);
        if (qtCurveStylePlugin != NULL) {
            QStyle* qtCurve = qtCurveStylePlugin->create("QtCurve");
            if (qtCurve != NULL) {
                QApplication::setStyle(qtCurve);
            }
            else
                printf("%s - style not created\n", __func__);
        }
        else
            printf("%s - stylePlugin not loaded\n", __func__);
    }
    else
        printf("%s - qtcurve.so file not loaded\n", __func__);

    MainWindow w;
    w.show();


    return a.exec();
}
