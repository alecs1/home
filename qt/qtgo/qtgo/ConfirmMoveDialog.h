#ifndef CONFIRMMOVEDIALOG_H
#define CONFIRMMOVEDIALOG_H

#include <QDialog>

namespace Ui {
class ConfirmMoveDialog;
}

class ConfirmMoveDialog : public QDialog
{
    Q_OBJECT

public:
    explicit ConfirmMoveDialog(QWidget *parent = 0);
    ~ConfirmMoveDialog();
    void setPixmap(QPixmap &pixmap);

private:
    Ui::ConfirmMoveDialog *ui;
    //QIcon icon;
};

#endif // CONFIRMMOVEDIALOG_H
