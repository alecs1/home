#include "PlayerWidget.h"
#include "ui_PlayerWidget.h"

#include <QPainter>

#include "Global.h"

PlayerWidget::PlayerWidget(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::PlayerWidget)
{
    printf("qtgo: %s\n", __func__);
    ui->setupUi(this);
    ui->playerComboBox->insertItem((int)PlayerType::AI, "Computer");
    ui->playerComboBox->insertItem((int)PlayerType::LocalHuman, "Human");
    ui->playerComboBox->insertItem((int)PlayerType::Network, "Network");
}

PlayerWidget::~PlayerWidget()
{
    delete ui;
}

int PlayerWidget::playerType() const {
    return ui->playerComboBox->currentIndex();
}

void PlayerWidget::setPlayerType(PlayerType type) {
    ui->playerComboBox->setCurrentIndex((int)type);
}

void PlayerWidget::setPixmap(QPixmap aPixmap) {
    printf("qtgo: %s - set pixmap size:%d, %d\n", __func__, aPixmap.size().width(), aPixmap.size().height());
    pixmap = aPixmap;
    ui->playerColourLabel->setMinimumSize(pixmap.size());
    ui->playerColourLabel->setPixmap(pixmap);
}

void PlayerWidget::enableChoosingPlayer(bool enable) {
    ui->playerComboBox->setEnabled(enable);
}
