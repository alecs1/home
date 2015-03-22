#include "HandicapDialog.h"
#include "ui_HandicapDialog.h"

#include "Handicap.h"

HandicapDialog::HandicapDialog(SGameSettings::Handicap &handicap, QWidget *parent) :
    QDialog(parent),
    ui(new Ui::HandicapDialog)
{
    ui->setupUi(this);
    ui->layout->addWidget(new Handicap(handicap, this), 2, 0, 1, 1);
}

HandicapDialog::~HandicapDialog()
{
    delete ui;
}
