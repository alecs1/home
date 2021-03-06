#include "MainWindow.h"
#include <QApplication>
#include <QPluginLoader>
#include <QStylePlugin>
#include <QCommonStyle>
#include <QStyle>

#include "GameStruct.h"
#include "Global.h"
#include "Logger.h"

//QtCurve causes start-up crashes on Android, and debugging on Android is such a pain
#define USE_QTCURVE 0
#undef USE_QTCURVE

#if defined(Q_OS_ANDROID) && defined(USE_QTCURVE)
namespace QtCurve {
    class Style : public QCommonStyle {
    public:
        Style();
    };

}
#endif

/**
 * Destructor will perform cleanup upon exit (flush Logger).
 */
class Cleaner {
public:
    ~Cleaner() {
        Logger::finish();
    }
};

int main(int argc, char *argv[])
{
    Logger::initLogging();

    Q_INIT_RESOURCE(res);

    QApplication a(argc, argv);

    //can't use "platformType()
#if defined(Q_OS_ANDROID) && defined(USE_QTCURVE)
        /**/
        printf("%s - waiting for debugger\n", __func__);
        bool waitingForDebugger = false;
        int waitCount = 0;
        while ((waitingForDebugger) && (waitCount < 60)) {
            sleep(1);
            waitCount += 1;
            printf("%s - step:%d\n\n\n", __func__, waitCount);
        }
        printf("%s - constructing QStyle\n", __func__);
        QtCurve::Style* qtCurve = new QtCurve::Style();
        printf("%s - calling setStyle\n", __func__);
        a.setStyle(qtCurve);
        printf("%s - done setting up style\n", __func__);
        /**/
#elif defined(USE_QTCURVE)
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

    Cleaner c;
    MainWindow w;
    w.show();

    return a.exec();
}
