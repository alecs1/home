#include "AddressDialog.h"
#include "ui_AddressDialog.h"

#include <QCompleter>

AddressDialog::AddressDialog(const QStringList completionList, QWidget* parent) :
    QDialog(parent),
    ui(new Ui::AddressDialog)
{
    ui->setupUi(this);

    if (completionList.length() > 0) {
        ui->addressEdit->setText(completionList[0]);
    }

    completer = new QCompleter(completionList, this);
    completer->setCaseSensitivity(Qt::CaseInsensitive);
    ui->addressEdit->setCompleter(completer);
}

AddressDialog::~AddressDialog() {
    delete ui;
    delete completer;
}

QString AddressDialog::address() const {
    return ui->addressEdit->text();
}
