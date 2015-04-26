#include "BusyDialog.h"
#include "ui_BusyDialog.h"

BusyDialog::BusyDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::BusyDialog)
{
    ui->setupUi(this);
    pixmap = new QPixmap(QString(":/resources/cursorBlackWhite.png"));
    ui->animationLabel->setPixmap(*pixmap);
}

BusyDialog::~BusyDialog()
{
    delete ui;
    delete pixmap;
}

void BusyDialog::setText(QString text) {
    ui->messageLabel->setText(text);
}

void BusyDialog::show() {
    QDialog::show();
    animationStep();
}

//not implemented, since we don't have an event loop yet
void BusyDialog::animationStep() {


}
