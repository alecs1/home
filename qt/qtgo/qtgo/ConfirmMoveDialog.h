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
    void setMinimalInterface(bool aMinimal = true);

public slots:
    void changeProgramSettings();

private:
    Ui::ConfirmMoveDialog *ui;
    QIcon* icon;
    bool minimal = false;
};

#endif // CONFIRMMOVEDIALOG_H
