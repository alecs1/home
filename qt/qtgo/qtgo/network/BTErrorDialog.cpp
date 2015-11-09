#include "BTErrorDialog.h"
#include "network/ui_BTErrorDialog.h"

BTErrorDialog::BTErrorDialog(QString text, QWidget *parent) :
    QDialog(parent),
    ui(new Ui::BTErrorDialog)
{
    ui->setupUi(this);
}

BTErrorDialog::~BTErrorDialog()
{
    delete ui;
}
