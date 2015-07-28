#ifdef WithQt5Quick
#include <QtQuick/QQuickView>
#include <QtQuickWidgets/QQuickWidget>
#endif

#include <QLayout>
#include <QPushButton>
#include <QLabel>
#include <QSpacerItem>
#include <QMessageBox>

//#include "Global.h"
#include "VERSION.h"
#include "AboutDialog.h"
#include "ThirdPartyInfo.h"



//find the main window so we can create the dialog as large as the main window is
#include <QMainWindow>
#include <QApplication>
static QMainWindow* getMainWindow()
{
    QWidgetList widgets = qApp->topLevelWidgets();
    for (QWidgetList::iterator i = widgets.begin(); i != widgets.end(); ++i)
        if ((*i)->objectName() == "MainWindow")
            return (QMainWindow*) (*i);
    return NULL;
}




AboutDialog::AboutDialog(QWidget *parent) : QDialog(parent) {
    printf("%s\n", __func__);
    if (layout() != NULL)
        delete layout();

    QGridLayout* gridLayout= new QGridLayout();
    setLayout(gridLayout);

    thirdPartyCreditsButton = new QPushButton("Third-party credits", this);
    gridLayout->addWidget(thirdPartyCreditsButton, 1, 0);

    debugButton = new QPushButton("Debug info", this);
    gridLayout->addWidget(debugButton, 1, 1);

    okButton = new QPushButton("Close", this);
    gridLayout->addWidget(okButton, 2, 0, 1, 2);

    connect(debugButton, SIGNAL(clicked()), this, SLOT(showDebugWindow()));
    connect(thirdPartyCreditsButton, SIGNAL(clicked()), this, SLOT(showThirdPartiesWindow()));

#ifdef WithQt5Quick
    quickWidget = new QQuickWidget();
    quickWidget->setSource(QUrl("qrc:/AboutDialog.qml"));
    quickWidget->setResizeMode(QQuickWidget::SizeRootObjectToView);
    gridLayout->addWidget(quickWidget, 0, 0, 1, 2);
#else
    QString labelContent = "<h2><b>FreeGo " + QString(FREEGO_VERSION) + "</b></hr></h2><br/>Written with the excellent Qt and CMake.<br/>Gameplay entirely provided by GNU Go.<br/>License GNU GPLv3.<br/>Source code at: <a href=\"https://github.com/alecs1/home/tree/master/qt/qtgo/\">https://github.com/alecs1/home/tree/master/qt/qtgo/</a>";
    androidLabel = new QLabel(labelContent);
    androidLabel->setAlignment(Qt::AlignCenter);
    gridLayout->addWidget(androidLabel, 0, 0, 1, 2);
#endif
    connect(okButton, SIGNAL(clicked()), this, SLOT(accept()));

    QMainWindow* mainWindow = getMainWindow();
    if (mainWindow != NULL) {
        resize(mainWindow->size());
    }
    else {
        printf("%s - error, could not find MainWindow\n", __func__);
    }
    printSizeInfo(__func__);
}

AboutDialog::~AboutDialog() {
    printSizeInfo(__func__);
#ifdef WithQt5Quick
    delete quickWidget; //will this delete twice on Desktop? we'll see :D
#endif
}

void AboutDialog::show() {
    printSizeInfo(__func__);
    QDialog::show();
    printSizeInfo(__func__);
}

int AboutDialog::exec() {
    printSizeInfo(__func__);
    int retVal = QDialog::exec();
    printSizeInfo(__func__);
    printf("%s - retVal=%d\n", __func__, retVal);
    return retVal;
}

void AboutDialog::resizeEvent(QResizeEvent* event) {
    printSizeInfo(__func__);
    QDialog::resizeEvent(event);
    printSizeInfo(__func__);
}

void AboutDialog::printSizeInfo(const char* func) const {
    printf("%s - sizes: window: (%d %d) %dx%d\n",
            func,
            x(), y(), width(), height());
#ifdef WithQt5Quick
    printf("\t\t quickWidget:(%d %d) %dx%d\n",
           quickWidget->x(), quickWidget->y(), quickWidget->width(), quickWidget->height()
           );
#endif
}

void AboutDialog::showDebugWindow() {
    QMessageBox::aboutQt(this);
}

void AboutDialog::showThirdPartiesWindow() {
    ThirdPartyInfo* info = new ThirdPartyInfo(this);

    QMainWindow* mainWindow = getMainWindow();
    if (mainWindow != NULL) {
        resize(mainWindow->size());
    }
    else {
        printf("%s - error, could not find MainWindow\n", __func__);
    }

    info->exec();
    delete info;
}


