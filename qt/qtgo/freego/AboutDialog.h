#ifndef ABOUTDIALOG_H
#define ABOUTDIALOG_H

class QQuickWidget;
class QQuickView;
class QLabel;

#include <QDialog>

class AboutDialog : public QDialog {
Q_OBJECT
public:
    AboutDialog(QWidget* parent=0);
    ~AboutDialog();
    void printSizeInfo(const char *func) const;
public slots:
    void show();
    int exec();
protected:
    void resizeEvent(QResizeEvent* event);

public slots:
    void showDebugWindow();
    void showThirdPartiesWindow();

private:
    QQuickWidget* quickWidget = NULL; //Android: https://bugreports.qt.io/browse/QTBUG-39454
    QLabel* androidLabel = NULL;
    QPushButton* okButton;
    QPushButton* thirdPartyCreditsButton;
    QPushButton* debugButton;
};


#endif // ABOUTDIALOG_H

