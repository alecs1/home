#include "GameInvitation.h"
#include "ui_GameInvitation.h"

GameInvitation::GameInvitation(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::GameInvitation)
{
    ui->setupUi(this);
}

GameInvitation::~GameInvitation()
{
    delete ui;
}

void GameInvitation::setImageWidget(QWidget* widget) {
    QVBoxLayout* aux = dynamic_cast<QVBoxLayout*>(layout());
    aux->insertWidget(0, widget);
}
