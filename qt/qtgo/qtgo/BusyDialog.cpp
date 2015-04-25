#include "BusyDialog.h"
#include "ui_BusyDialog.h"

BusyDialog::BusyDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::BusyDialog)
{
    ui->setupUi(this);
}

BusyDialog::~BusyDialog()
{
    delete ui;
}

void BusyDialog::show() {
    QDialog::show();
    animationStep();
}

void BusyDialog::animationStep() {


}
