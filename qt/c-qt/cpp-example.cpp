#include <QPushButton>
#include <QApplication>
#include <QObject>

#include <stdio.h>

int main(int argc, char* argv[]) {
    QApplication app(argc, argv);

    QPushButton button("Close");
    QObject::connect(&button, SIGNAL(clicked()), &app, SLOT(qui
t()));

    printf("before show()\n");
    button.show();
    printf("after show()\n");
    int retVal = -1;
    retVal = app.exec();
    printf("Exiting with retVal=%d\n", retVal);
    return retVal;
}
