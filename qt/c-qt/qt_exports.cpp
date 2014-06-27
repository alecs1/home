#include "qt_exports.h"

#include <QPushButton>
#include <QApplication>

QTContainer* C_QPushButton(char* text, QTContainer* parent) {
    QWidget* aParent = NULL;
    if (parent != NULL)
        aParent = reinterpret_cast<QWidget*>(parent->qtObj); //TODO - do i need to treat the NULL case? type safety is screwed anyway
    QPushButton* button = new QPushButton(text, aParent);
    QTContainer* ret = (QTContainer*)(malloc(sizeof(QTContainer)));
    ret->qtObj = reinterpret_cast<void*>(button);
    ret->type = EC_QPushButton;
    return ret;
}
    

void C_QWidget_Show(QTContainer* widget) {
    QWidget* aWidget = reinterpret_cast<QWidget*>(widget->qtObj);
    aWidget->show();
}

//Big warning - argc - since we're passing by reference, not value, there's no guaranty about when the value could be used; it must remain valid throughout the existance of the QApplication
QTContainer* C_QApplication(int* argc, char* argv[]) {
    QApplication* app = new QApplication(*argc, argv);
    QTContainer* ret = (QTContainer*)(malloc(sizeof(QTContainer)));
    ret->qtObj = reinterpret_cast<void*>(app);
    ret->type = EC_QApplication;
    return ret;
}

EXTERN_C int C_QApplication_Exec(QTContainer* app) {
    QApplication* aApp = reinterpret_cast<QApplication*>(app->qtObj);
    int retVal = aApp->exec();
    return retVal;
}

//this is really tricky now, since it already depends on the preprocessor; skip for the moment
int c_connect(QTContainer* src, void* signal, QTContainer* dest, void* slot) {
return -1;
}
