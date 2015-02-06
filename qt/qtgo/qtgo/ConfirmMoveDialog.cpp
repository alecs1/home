#include "ConfirmMoveDialog.h"
#include "ui_ConfirmMoveDialog.h"

ConfirmMoveDialog::ConfirmMoveDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::ConfirmMoveDialog)
{
    ui->setupUi(this);
    QObject::connect(ui->confirmButton, SIGNAL(clicked()), this, SLOT(accept()));
    QObject::connect(ui->cancelButton, SIGNAL(clicked()), this, SLOT(reject()));
}

ConfirmMoveDialog::~ConfirmMoveDialog()
{
    delete ui;
}
