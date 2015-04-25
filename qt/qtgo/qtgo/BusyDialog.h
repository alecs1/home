#ifndef BUSYDIALOG_H
#define BUSYDIALOG_H

#include <QDialog>

namespace Ui {
class BusyDialog;
}

//this is a busy dialog to show a rotating cursorBlackWhite.svg image. I'll just use QProgressDialog for now.
class BusyDialog : public QDialog
{
    Q_OBJECT

public:
    explicit BusyDialog(QWidget *parent = 0);
    ~BusyDialog();
    void show();

private:
    void animationStep();

private:
    Ui::BusyDialog *ui;
    QPixmap* pixmap;
};

#endif // BUSYDIALOG_H
