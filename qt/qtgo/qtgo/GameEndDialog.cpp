#include "GameEndDialog.h"
#include "ui_GameEndDialog.h"

GameEndDialog::GameEndDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::GameEndDialog)
{
    ui->setupUi(this);
}

GameEndDialog::~GameEndDialog()
{
    delete ui;
}


void GameEndDialog::setText(QString text) {
    ui->endGameText->setText(text);
}

void GameEndDialog::setPixmap(QPixmap& pixmap) {
    ui->winnerPixmap->setPixmap(pixmap);
}
