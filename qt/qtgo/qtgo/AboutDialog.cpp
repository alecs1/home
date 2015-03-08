#include <QDialog>
#include <QtQuick/QQuickView>
#include <QtQuickWidgets/QQuickWidget>
#include <QLayout>
#include <QPushButton>

#include "Global.h"
#include "AboutDialog.h"

AboutDialog::AboutDialog(QWidget *parent) : QDialog(parent) {
    printf("%s\n", __func__);
    if (layout() != NULL)
        delete layout();

    QGridLayout* gridLayout= new QGridLayout();
    setLayout(gridLayout);

    okButton = new QPushButton("Close", this);
    gridLayout->addWidget(okButton, 1, 0);

    if (true) {
        quickWidget = new QQuickWidget();
        //quickWidget->setSource(QUrl("qrc:/AboutDialog.qml"));
        quickWidget->setSource(QUrl("qrc:/Example.qml"));
        quickWidget->setResizeMode(QQuickWidget::SizeRootObjectToView);
        quickWidget->resize(100, 100);

        gridLayout->addWidget(quickWidget, 0, 0);
    }
    else {
        quickWidget = new QQuickWidget();
    }

    resize(800, 800);
    printSizeInfo(__func__);
}

AboutDialog::~AboutDialog() {
    printSizeInfo(__func__);
    delete quickView;
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

