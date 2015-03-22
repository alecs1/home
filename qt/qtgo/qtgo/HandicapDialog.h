#ifndef HANDICAPDIALOG_H
#define HANDICAPDIALOG_H

#include <QDialog>

#include "Global.h"

namespace Ui {
class HandicapDialog;
}

class HandicapDialog : public QDialog
{
    Q_OBJECT

public:
    explicit HandicapDialog(SGameSettings::Handicap& handicap, QWidget *parent = 0);
    ~HandicapDialog();

private:
    Ui::HandicapDialog *ui;
};

#endif // HANDICAPDIALOG_H
