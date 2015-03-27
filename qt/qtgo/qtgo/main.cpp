#include "mainwindow.h"
#include <QApplication>
#include <QPluginLoader>
#include <QStylePlugin>
#include <QCommonStyle>
#include <QStyle>

#include "GameStruct.h"
#include "Global.h"

#if defined(Q_OS_ANDROID)
namespace QtCurve {
    class Style : public QCommonStyle {
    public:
        Style();
    };

}
#endif

int main(int argc, char *argv[])
{
    printf("\n\n\n\n\n\n\n\n%s - STAAAAARRTIIIIIIING\n\n\n\n\n\n\n\n", __func__);

    Q_INIT_RESOURCE(res);
    //game.size = 19;

    printf("%s - init QApplication\n", __func__);
    QApplication a(argc, argv);
    printf("%s - init QApplication-done\n", __func__);

    //dynamic loading on pc:
#if defined(Q_OS_ANDROID)
        /**/
        printf("%s - constructing QStyle\n", __func__);
        QtCurve::Style* qtCurve = new QtCurve::Style();
        //QtCurve::Style* qtCurve = NULL;
        printf("%s - calling setStyle\n", __func__);
        a.setStyle(qtCurve);
        printf("%s - done setting up style\n", __func__);
        /**/
#else
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
#endif

    MainWindow w;
    w.show();


    return a.exec();
}
