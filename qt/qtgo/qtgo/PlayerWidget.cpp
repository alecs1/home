#include "PlayerWidget.h"
#include "ui_PlayerWidget.h"

#include <QPainter>
#include <QMenu>

#include "Global.h"

PlayerWidget::PlayerWidget(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::PlayerWidget)
{
    printf("%s\n", __func__);
    ui->setupUi(this);
    ui->playerComboBox->insertItem((int)PlayerType::AI, "Computer (weak)");
    ui->playerComboBox->insertItem((int)PlayerType::LocalHuman, "Human");
    ui->playerComboBox->insertItem((int)PlayerType::Network, "Network");

    connect(ui->playerComboBox, SIGNAL(currentIndexChanged(int)), this, SIGNAL(playerTypeChanged(int)));
    connect(ui->playerComboBox, SIGNAL(currentIndexChanged(int)), this, SLOT(setPlayerTypeInt(int)));
    connect(ui->playerSettingsButton, SIGNAL(clicked()), this, SLOT(showMenuExplicit()));
    connect(ui->playerComboBox, SIGNAL(activated(int)), this, SLOT(showMenu(int)));


    AIMenu = new QMenu(this);
    AIMenu->setTitle("Computer strength");
    QAction* titleAct = AIMenu->addAction("Computer strength");
    titleAct->setEnabled(false);
    //TODO - on Android can't show too many levels. Fix.
    QAction* defAct = AIMenu->addAction("0 - Weak");
    AIMenu->setDefaultAction(defAct);
    for(int i = 1; i <= 4; i++) {
        AIMenu->addAction(QString::number(i));
    }
    AIMenu->addAction("5 - Strong");
    connect(AIMenu, SIGNAL(triggered(QAction*)), this, SLOT(AIActionActivated(QAction*)));
}

PlayerWidget::~PlayerWidget()
{
    delete ui;
}

int PlayerWidget::playerType() const {
    return ui->playerComboBox->currentIndex();
}

int PlayerWidget::getAIStrength() const {
    //because we have to show a limited number of options on Android screens
    return AIStrength * 2;
}

void PlayerWidget::setAIStrength(int strength) {
    AIActionActivated(AIMenu->actions()[strength/2 + 1]);
}

void PlayerWidget::setPlayerTypeInt(int type) {
    PlayerType auxType = (PlayerType)type;
    setPlayerType(auxType);
}

void PlayerWidget::setPlayerType(PlayerType type) {
    //printf("%s - type=%d\n", __func__, type);
    ui->playerComboBox->setCurrentIndex((int)type);
    ui->playerSettingsButton->show();
    ui->playerLabel->hide();
    return;

    //TODO - tried to make a  label and a toolbuton fill the same space and show them alternatively, if there's something to click or not.
    //doesn't work.
    if (type == PlayerType::AI) {
        ui->playerSettingsButton->show();
        printf("%s - playerSettingsButton: %dx%d\n", __func__,
               ui->playerSettingsButton->size().width(), ui->playerSettingsButton->size().height());
        printf("%s - playerLabel: %dx%d\n", __func__,
               ui->playerLabel->size().width(), ui->playerLabel->size().height());
        ui->playerLabel->hide();
    }
    else {
        ui->playerLabel->show();
        ui->playerLabel->resize(ui->playerSettingsButton->size());
        printf("%s - playerSettingsButton: %dx%d\n", __func__,
               ui->playerSettingsButton->size().width(), ui->playerSettingsButton->size().height());
        printf("%s - playerLabel: %dx%d\n", __func__,
               ui->playerLabel->size().width(), ui->playerLabel->size().height());
        ui->playerSettingsButton->hide();
    }
}

void PlayerWidget::setPixmap(QPixmap aPixmap) {
    printf("%s - set pixmap size:%d, %d\n", __func__, aPixmap.size().width(), aPixmap.size().height());
    pixmap = aPixmap;

    ui->playerSettingsButton->setMinimumSize(pixmap.size());
    ui->playerSettingsButton->setIconSize(pixmap.size());
    ui->playerSettingsButton->setIcon(QIcon(pixmap));

    ui->playerLabel->setPixmap(pixmap);
    ui->playerLabel->resize(ui->playerSettingsButton->size());
}

void PlayerWidget::enableChoosingPlayer(bool enable) {
    ui->playerComboBox->setEnabled(enable);
}

void PlayerWidget::showMenuExplicit() {
    showMenu(playerType());
}

void PlayerWidget::showMenu(int playerTypeInt) {
    printf("%s - playerType=%d\n", __func__, playerTypeInt);
    PlayerType type = (PlayerType)playerTypeInt;

    if (type == PlayerType::AI) {
        //AIMenu->setWindowFlags(Qt::FramelessWindowHint | AIMenu->windowFlags());
        AIMenu->show();
        QPoint globalPos = mapToGlobal(QPoint(0, 0));
        globalPos.setX(globalPos.x() - AIMenu->size().width());
        AIMenu->move(globalPos);
    }
    else
        AIMenu->hide();
}

void PlayerWidget::AIActionActivated(QAction* action) {
    //first action is the title
    for(int i = 1; i < AIMenu->actions().size(); i++) {
        if (action == AIMenu->actions()[i]) {
            AIMenu->setDefaultAction(action);
            AIStrength = i - 1;
            ui->playerComboBox->setItemText((int)PlayerType::AI,
                                            QString("Computer (lev. " + QString::number(AIStrength) + QString(")")));
            emit playerStrengthChanged(getAIStrength());
            printf("%s - AIStrength=%d\n", __func__, AIStrength);
            break;
        }
    }
}
