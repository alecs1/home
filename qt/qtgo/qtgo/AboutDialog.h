#ifndef ABOUTDIALOG_H
#define ABOUTDIALOG_H

class QQuickWidget;
class QQuickView;
class QLabel;

#include <QDialog>

class AboutDialog : public QDialog {
public:
    AboutDialog(QWidget* parent=0);
    ~AboutDialog();
    void printSizeInfo(const char *func) const;
public slots:
    void show();
    int exec();
protected:
    void resizeEvent(QResizeEvent* event);



private:
    //QQmlEngine* qmlEngine;
    //QQmlComponent* component;
    //QQuickView* quickView = NULL; //desktop
    QQuickWidget* quickWidget = NULL; //Android: https://bugreports.qt.io/browse/QTBUG-39454
    QLabel* androidLabel = NULL;
    QPushButton* okButton;
};


#endif // ABOUTDIALOG_H

