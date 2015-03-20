#include <QSvgRenderer>
#include <QPainter>
#include <QMenu>
#include <QMessageBox>

#include "GameSettings.h"
#include "ui_GameSettings.h"

#include "PlayerWidget.h"
#include "GameStruct.h"
#include "ConfirmMoveDialog.h"
#include "RoundInfo.h"
#include "AboutDialog.h"

GameSettings::GameSettings(QWidget *parent):
    QWidget(parent),
    ui(new Ui::GameSettings())
{
    printf("gtgo: %s - start\n", __func__);
    ui->setupUi(this);

    //initialize the two players:
    QSvgRenderer svgR;
    QFont font;
    int defaultFontSize = font.pixelSize();
    if (defaultFontSize <= 0)
        defaultFontSize = font.pointSize();
    if (defaultFontSize <= 0) {
        printf("%s - error - could not establish a fonst size!\n", __func__);
    }

    const float SCALE= 2.5;
    int diameter = SCALE * defaultFontSize;
    resize(diameter, diameter);

    printf("%s - defaultFontSize=%d, diameter=%d\n", __func__, defaultFontSize, diameter);

    QPixmap blackStone(diameter, diameter);
    blackStone.fill(Qt::transparent);
    svgR.load(QString(":/resources/cursorBlack.svg"));
    QPainter bPainter(&blackStone);
    svgR.render(&bPainter);

    QPixmap whiteStone(diameter, diameter);
    whiteStone.fill(Qt::transparent);
    svgR.load(QString(":/resources/cursorWhite.svg"));
    QPainter wPainter(&whiteStone);
    svgR.render(&wPainter);


    blackPlayer = new PlayerWidget(this);
    ui->playersLayout->addWidget(blackPlayer);
    blackPlayer->setPlayerType(settings.black);
    blackPlayer->setPixmap(blackStone);

    whitePlayer = new PlayerWidget(this);
    ui->playersLayout->addWidget(whitePlayer);
    whitePlayer->setPlayerType(settings.white);
    whitePlayer->setPixmap(whiteStone);

    mainMenu = new QMenu(this);
    mainMenu->addAction("Help");
    saveGameAction = mainMenu->addAction("Save game");
    loadGameAction = mainMenu->addAction("Open saved game");
    aboutAction = mainMenu->addAction("About");
    debugAction = mainMenu->addAction("Debug helper");

    connect(ui->launchButton, SIGNAL(clicked()), this, SLOT(launchGameClicked()));
    connect(ui->finishButton, SIGNAL(clicked()), this, SLOT(askConfirmFinishGame()));
    connect(ui->scoreEstimateButton, SIGNAL(clicked()), this, SLOT(toggleShowEstimateScore()));

    connect(blackPlayer, SIGNAL(playerTypeChanged(int)), this, SLOT(populateSettings()));
    connect(whitePlayer, SIGNAL(playerTypeChanged(int)), this, SLOT(populateSettings()));
    connect(blackPlayer, SIGNAL(playerStrengthChanged(int)), this, SLOT(populateSettings()));
    connect(whitePlayer, SIGNAL(playerStrengthChanged(int)), this, SLOT(populateSettings()));
    connect(ui->button9x9, SIGNAL(toggled(bool)), this, SLOT(populateSettings()));
    connect(ui->button13x13, SIGNAL(toggled(bool)), this, SLOT(populateSettings()));
    connect(ui->button19x19, SIGNAL(toggled(bool)), this, SLOT(populateSettings()));
    connect(ui->passButton, SIGNAL(clicked()), this, SIGNAL(userPassedMove()));
    connect(ui->undoButton, SIGNAL(clicked()), this, SIGNAL(undoMove()));
    connect(ui->hintButton, SIGNAL(clicked()), this, SIGNAL(showHints()));
    connect(ui->menuLauncher1, SIGNAL(clicked()), this, SLOT(showMenu()));
    connect(ui->menuLauncher2, SIGNAL(clicked()), this, SLOT(showMenu()));
    connect(saveGameAction, SIGNAL(triggered()), this, SIGNAL(saveGame()));
    connect(loadGameAction, SIGNAL(triggered()), this, SIGNAL(loadGame()));
    connect(aboutAction, SIGNAL(triggered()), this, SLOT(showAbout()));
    connect(debugAction, SIGNAL(triggered()), this, SLOT(showDebugWindow()));

    ui->button19x19->setChecked(true);

    populateSettings();

    roundInfo = new RoundInfo(this);
    ui->topRow->insertWidget(0, roundInfo);
    showingRoundInfo = false;
    roundInfo->hide();
    printf("%s - roundInfo:%dx%d\n",
           __func__, roundInfo->width(), roundInfo->height());


    ui->hintButton->hide();
    ui->passButton->hide();
    ui->undoButton->hide();

    showScore = false;
    setGameState(GameState::Initial);

    confirmMoveDialog = NULL;

    //TODO: need to make a QToolButton look as much as possible as an Android menu launcher
    //on Android this thing is called "action bar"
    float MENU_SCALE = 2.5;
    ui->menuLauncher1->setMinimumSize(MENU_SCALE * defaultFontSize, MENU_SCALE * defaultFontSize);
    ui->menuLauncher2->setMinimumSize(MENU_SCALE * defaultFontSize, MENU_SCALE * defaultFontSize);
    ui->menuLauncher1->hide();

    printf("%s - end\n", __func__);
}

GameSettings::~GameSettings() {
    delete confirmMoveDialog;
}

void GameSettings::setGameState(GameState state) {
    gameState = state;
    if (state == GameState::Resumed) {
        roundInfo->show();
        ui->launchButton->setText("Resume");
        ui->finishButton->show();
        ui->menuLauncher1->show();
        ui->menuLauncher2->hide();
        ui->tableSizeGroupBox->hide();
        ui->hintButton->hide();
        ui->passButton->hide();
        whitePlayer->enableChoosingPlayer(true);
        blackPlayer->enableChoosingPlayer(true);
        ui->scoreEstimateButton->show();
        showingRoundInfo = true;
    }
    else if (state == GameState::Initial) {
        ui->scoreEstimateButton->hide();
        ui->finishButton->hide();
        setScoreEstimate(0);
    }
    else if (state == GameState::Started) {
        ui->launchButton->hide();
        ui->finishButton->show();
        roundInfo->show();
        ui->menuLauncher1->show();
        ui->menuLauncher2->hide();
        ui->tableSizeGroupBox->hide();
        ui->hintButton->show();
        ui->passButton->show();
        ui->undoButton->show();
        whitePlayer->enableChoosingPlayer(false);
        blackPlayer->enableChoosingPlayer(false);
        ui->scoreEstimateButton->show();
        setScoreEstimate(0);
        showingRoundInfo = true;//TODO - get rid of this variable
    }
    else if (state == GameState::Stopped) {
        ui->launchButton->show();
        ui->launchButton->setText("Start");
        ui->finishButton->hide();
        if (showingRoundInfo == true) {
            roundInfo->hide();
            ui->menuLauncher1->hide();
        }
        ui->menuLauncher2->show();
        ui->tableSizeGroupBox->show();
        ui->hintButton->hide();
        ui->passButton->hide();
        ui->undoButton->hide();
        ui->undoButton->setEnabled(true);
        whitePlayer->enableChoosingPlayer(true);
        blackPlayer->enableChoosingPlayer(true);
        //ui->scoreEstimateButton->hide();
        showingRoundInfo = false;
    }
}

void GameSettings::setScoreEstimate(float score) {
    scoreEstimate = score;
    updateScoreEstimateButton();
}

void GameSettings::updateScoreEstimateButton() {
    QString text;
    if (showScore) {
        text = "White: ";
        if (scoreEstimate < 0)
            text = "Black: ";

        QString aux;
        aux.sprintf("%3.1f", fabs(scoreEstimate));
        text += aux + " (hide)";
    }
    else {
        text = "Estimate score";
    }

    ui->scoreEstimateButton->setText(text);
}

void GameSettings::setCurrentPlayer(int player, PlayerType type) {
    roundInfo->setCurrentPlayer(player, type);
    roundInfo->update();

    bool enableBlockingGroup = true;
    if (type == PlayerType::AI)
        enableBlockingGroup = false;

    ui->scoreEstimateButton->setEnabled(enableBlockingGroup);
    ui->hintButton->setEnabled(enableBlockingGroup);
    ui->passButton->setEnabled(enableBlockingGroup);
    ui->undoButton->setEnabled(enableBlockingGroup);
}

void GameSettings::showConfirmButton(bool show, int colour) {
    //printf("%s - show=%d\n", __func__, show);
    if (show == false) {
        if (confirmMoveDialog != NULL) {
            confirmMoveDialog->hide();
        }
        return;
    }
    if ((gameState != GameState::Started) && (gameState != GameState::Initial) && (gameState != GameState::Resumed))
        return;

    if (confirmMoveDialog == NULL) {
        confirmMoveDialog = new ConfirmMoveDialog(this);
        connect(confirmMoveDialog, SIGNAL(finished(int)), this, SIGNAL(userConfirmedMove(int)));
    }
    //show dialog over the settings UI, but somehow seems hackish; just replace instead of creating a new window.
    QPoint globalPos = mapToGlobal(QPoint(0, 0));
    confirmMoveDialog->setGeometry(QRect(globalPos, this->size()));
    confirmMoveDialog->setWindowFlags(Qt::FramelessWindowHint | confirmMoveDialog->windowFlags());

    int diameter =this->size().width();
    QSvgRenderer svgR;
    QPixmap stone(diameter, diameter);
    stone.fill(Qt::transparent);
    if (colour == BLACK) {
        svgR.load(QString(":/resources/cursorBlack.svg"));
    }
    else if (colour == WHITE){
        svgR.load(QString(":/resources/cursorWhite.svg"));
    }
    else {
        printf("%s - why are we setting and colour %d?\n", __func__, colour);
    }
    QPainter bPainter(&stone);
    svgR.render(&bPainter);

    confirmMoveDialog->setPixmap(stone);
    confirmMoveDialog->show();
    confirmMoveDialog->raise();
    confirmMoveDialog->activateWindow();
}

void GameSettings::toggleShowEstimateScore() {
    if (showScore)
        showScore = false;
    else
        showScore = true;

    updateScoreEstimateButton();
    emit doEstimateScore(showScore);
}

void GameSettings::showMenu() {
    printf("%s\n", __func__);
    mainMenu->show();
    //now which of the the two guys is visible?
    QToolButton* menuLauncher = ui->menuLauncher1;
    if (menuLauncher->isVisible() == false)
        menuLauncher = ui->menuLauncher2;
    QPoint globalPos = menuLauncher->mapToGlobal(QPoint(0, 0));
    globalPos.setX(globalPos.x() - mainMenu->size().width());
    mainMenu->move(globalPos);
}

#include <QtQuick/QQuickView>
void GameSettings::showAbout() {
    AboutDialog dialog;
    dialog.exec();
    //QQuickView* view = new QQuickView();
    //view->setSource(QUrl("qrc:/Example.qml"));
    //view->show();
    printf("%s - done\n", __func__);
}

void GameSettings::showDebugWindow() {
    QMessageBox::aboutQt(this);
}

bool operator==(const SGameSettings& s1, const SGameSettings& s2) {
    if (s1.blackAIStrength != s2.blackAIStrength)
        return false;
    if (s1.whiteAIStrength != s2.whiteAIStrength)
        return false;
    if (s1.size != s2.size)
        return false;
    if (s1.black != s2.black)
        return false;
    if (s1.white != s2.white)
        return false;
    return true;
}

void GameSettings::populateSettings() {
    printf("%s\n", __func__);
    SGameSettings newSettings;
    newSettings.size = 19;
    if (ui->button9x9->isChecked())
        newSettings.size = 9;
    else if (ui->button13x13->isChecked())
        newSettings.size = 13;
    else if (ui->button19x19->isChecked())
        newSettings.size = 19;

    newSettings.black = (PlayerType)blackPlayer->playerType();
    newSettings.blackAIStrength = blackPlayer->getAIStrength();
    newSettings.white = (PlayerType)whitePlayer->playerType();
    newSettings.whiteAIStrength = whitePlayer->getAIStrength();

    if (newSettings == settings)
        return;
    else {
        printf("%s - settings have changed\n", __func__);
        settings = newSettings;
        emit gameSettingsChanged(settings);
    }
}

void GameSettings::receiveSettings(SGameSettings settings) {
    printf("%s\n", __func__);
    switch (settings.size) {
        case 9:
            ui->button9x9->setChecked(true);
            break;
        case 13:
            ui->button13x13->setChecked(true);
            break;
        case 19:
            ui->button19x19->setChecked(true);
            break;
    }

    blackPlayer->setPlayerType(settings.black);
    blackPlayer->setAIStrength(settings.blackAIStrength);
    whitePlayer->setPlayerType(settings.white);
    whitePlayer->setAIStrength(settings.whiteAIStrength);

}

void GameSettings::askConfirmFinishGame() {
    int ret = QMessageBox::question(this, "FreeGo", "Do you want to finish the game?",
                                    QMessageBox::Cancel | QMessageBox::Ok);
    if (ret == QMessageBox::Ok)
        emit finishGamePerform();
}

void GameSettings::launchGameClicked() {
    populateSettings();
    emit launchGamePerform(settings);
}

