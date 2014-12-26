#include "PlayerWidget.h"
#include "ui_PlayerWidget.h"

#include "Global.h"

PlayerWidget::PlayerWidget(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::PlayerWidget)
{
    ui->setupUi(this);
    ui->playerComboBox->insertItem((int)PlayerType::AI, "Computer");
    ui->playerComboBox->insertItem((int)PlayerType::LocalHuman, "Human");
    ui->playerComboBox->insertItem((int)PlayerType::Network, "Network");
}

PlayerWidget::~PlayerWidget()
{
    delete ui;
}

void PlayerWidget::setTitle(QString title) {
    ui->playerGroupBox->setTitle(title);
}

int PlayerWidget::playerType() const {
    return ui->playerComboBox->currentIndex();
}

void PlayerWidget::setPlayerType(PlayerType type) {
    ui->playerComboBox->setCurrentIndex((int)type);
}
