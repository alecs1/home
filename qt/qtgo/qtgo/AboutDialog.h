#ifndef ABOUTDIALOG_H
#define ABOUTDIALOG_H

class QQmlEngine;
class QQmlComponent;
class QQuickView;

#include <QDialog>

class AboutDialog : public QDialog {
public:
    AboutDialog();
    ~AboutDialog();
//    QSize sizeHint();

private:
    //QQmlEngine* qmlEngine;
    //QQmlComponent* component;
    QQuickView* quickView;
    QWidget* container;

public slots:
    //int exec();
};


#endif // ABOUTDIALOG_H

