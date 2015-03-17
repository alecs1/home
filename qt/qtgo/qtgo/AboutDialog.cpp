#include <QDialog>
#include <QtQuick/QQuickView>
#include <QtQuickWidgets/QQuickWidget>
#include <QLayout>
#include <QPushButton>
#include <QLabel>
#include <QSpacerItem>

#include "Global.h"
#include "AboutDialog.h"

//find the main window so we can create the dialog as large as the main window is
#include <QMainWindow>
#include <QApplication>

QMainWindow* getMainWindow()
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

    okButton = new QPushButton("Close", this);
    gridLayout->addWidget(okButton, 1, 0);

    if ( (platformType() != PlatformType::Android) && true) {
        quickWidget = new QQuickWidget();
        quickWidget->setSource(QUrl("qrc:/AboutDialog.qml"));
        //quickWidget->setSource(QUrl("qrc:/Example.qml"));
        quickWidget->setResizeMode(QQuickWidget::SizeRootObjectToView);
        gridLayout->addWidget(quickWidget, 0, 0);
    }
    else {
        quickWidget = new QQuickWidget(); //unused, just initialised
        androidLabel = new QLabel("<h2><b>FreeGo</b></hr></h2><br/>Written with the excellent Qt and CMake.<br/>Gameplay entirely provided by GNU Go.<br/>License GNU GPLv3.<br/>Source code at: <a href=\"https://github.com/alecs1/home/tree/master/qt/qtgo/\">https://github.com/alecs1/home/tree/master/qt/qtgo/</a>");
        androidLabel->setAlignment(Qt::AlignCenter);
        gridLayout->addWidget(androidLabel, 0, 0);
        //gridLayout->setRowMinimumHeight(2, 50); //that Close button is too low.
        //setWindowState(windowState() | Qt::WindowFullScreen);
    }
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
    delete quickWidget; //will this delete twice on Desktop? we'll see :D
}

void AboutDialog::show() {
    printSizeInfo(__func__);
    QDialog::show();
    printSizeInfo(__func__);
}

//QSize AboutDialog::sizeHint() {
//    return QSize(400, 400);
//}

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
    printf("%s - sizes: window: (%d %d) %dx%d;\n\t\t quickWidget:(%d %d) %dx%d\n",
           func,
           x(), y(), width(), height(),
           quickWidget->x(), quickWidget->y(), quickWidget->width(), quickWidget->height()
           );
}

