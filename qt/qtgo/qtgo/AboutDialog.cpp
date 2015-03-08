#include <QDialog>
#include <QtQuick/QQuickView>
#include <QLayout>

#include "Global.h"
#include "AboutDialog.h"

AboutDialog::AboutDialog() {
    printf("%s\n", __func__);
    //engine = new QQmlEngine;
    //component = new QQmlComponent(&engine, ":/AboutDialog.qml");
    quickView = new QQuickView();
    quickView->setSource(QUrl("qrc:/AboutDialog.qml"));

    container = QWidget::createWindowContainer(quickView, this);
    printf("%s, layout=%p\n", __func__, layout());
    QLayout* auxLayout = layout();
    if (auxLayout == NULL) {
        auxLayout = new QGridLayout();
        setLayout(auxLayout);
    }
    layout()->addWidget(container);
    resize(800, 800);
}

AboutDialog::~AboutDialog() {
    printf("%s\n", __func__);
    delete quickView;
}

//QSize AboutDialog::sizeHint() {
//    return QSize(400, 400);
//}

//int AboutDialog::exec() {

//}
