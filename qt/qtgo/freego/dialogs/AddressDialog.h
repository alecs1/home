#pragma once

#include <QDialog>

class QCompleter;

namespace Ui {
class AddressDialog;
}

class AddressDialog : public QDialog
{
Q_OBJECT

public:
    explicit AddressDialog(const QStringList completionList, QWidget *parent = 0);
    ~AddressDialog();
    QString address() const;


private:
    Ui::AddressDialog *ui;
    QCompleter* completer;
};
