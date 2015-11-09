#ifndef BTERRORDIALOG_H
#define BTERRORDIALOG_H

#include <QDialog>

namespace Ui {
class BTErrorDialog;
}

class BTErrorDialog : public QDialog
{
    Q_OBJECT

public:
    explicit BTErrorDialog(QString text, QWidget *parent = 0);
    ~BTErrorDialog();

private:
    Ui::BTErrorDialog *ui;
};

#endif // BTERRORDIALOG_H
