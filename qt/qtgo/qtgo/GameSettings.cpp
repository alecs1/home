#include <QSvgRenderer>
#include <QPainter>
#include <QMenu>
#include <QMessageBox>
#include <cmath>

#include "GameSettings.h"
#include "ui_GameSettings.h"

#include "PlayerWidget.h"
#include "GameStruct.h"
#include "ConfirmMoveDialog.h"
#include "RoundInfo.h"
#include "AboutDialog.h"
#include "HelpDialog.h"
#include "HandicapDialog.h"
#include "SettingsDialog.h"

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
    saveGameAction = mainMenu->addAction("Save game");
    loadGameAction = mainMenu->addAction("Open saved game");
    settingsAction = mainMenu->addAction("Settings");
    connectBTAction = mainMenu->addAction("Bluetooth");
    connectTCPAction = mainMenu->addAction("TCP");
    debugBTAction = mainMenu->addAction("Bluetooth chat");
    helpAction = mainMenu->addAction("Help");
    aboutAction = mainMenu->addAction("About");
    smallInterfaceAction = mainMenu->addAction("Make table larger\n"
                                               "on small screens");

//TODO - why did the DEBUG define disappear?
#define DEBUG 1
#ifndef DEBUG
    connectBTAction->setEnabled(false);
    connectTCPAction->setEnabled(false);
    debugBTAction->setEnabled(false);
#endif


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
    connect(ui->handicapButton, SIGNAL(clicked()), this, SLOT(showHandicapWindow()));
    connect(ui->menuLauncher1, SIGNAL(clicked()), this, SLOT(showMenu()));
    connect(ui->menuLauncher2, SIGNAL(clicked()), this, SLOT(showMenu()));
    connect(saveGameAction, SIGNAL(triggered()), this, SIGNAL(saveGame()));
    connect(loadGameAction, SIGNAL(triggered()), this, SIGNAL(loadGame()));
    connect(aboutAction, SIGNAL(triggered()), this, SLOT(showAbout()));
    connect(helpAction, SIGNAL(triggered()), this, SLOT(showHelp()));
    connect(smallInterfaceAction, SIGNAL(triggered(bool)), this, SIGNAL(setMinimalInterface()));
    connect(connectBTAction, SIGNAL(triggered(bool)), this, SIGNAL(connectBT()));
    connect(connectTCPAction, SIGNAL(triggered(bool)), this, SIGNAL(connectTCP()));
    connect(debugBTAction, SIGNAL(triggered(bool)), this, SIGNAL(debugBT()));
    connect(settingsAction, SIGNAL(triggered()), this, SLOT(showSettings()));

    ui->button19x19->setChecked(true);

    populateSettings();
    updateHandicap(settings.handicap);

    //TODO - this is another Qt bug on Android.
    if (platformType() == PlatformType::Android)
        ui->handicapButton->setStyleSheet("");

    roundInfo = new RoundInfo(this);
    ui->topRow->insertWidget(0, roundInfo);
    roundInfoVisible = false;
    roundInfo->hide();
    printf("%s - roundInfo:%dx%d\n",
           __func__, roundInfo->width(), roundInfo->height());


    ui->hintButton->hide();
    ui->passButton->hide();
    ui->undoButton->hide();

    //ui->komiEdit->hide();

    scoreVisible = false;
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

//remove the roundInfo from the layout but stil own it;
RoundInfo* GameSettings::popRoundInfo() {
    ui->topRow->removeWidget(roundInfo);
    return roundInfo;
}

void GameSettings::pushBackRoundInfo() {
    ui->topRow->insertWidget(0, roundInfo);
    roundInfo->setVisible(roundInfoVisible);
    ui->topRow->update(); //probably Qt bug, need to update explicitly
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
        ui->handicapButton->hide();
        ui->hintButton->hide();
        ui->passButton->hide();
        whitePlayer->enableChoosingPlayer(true);
        blackPlayer->enableChoosingPlayer(true);
        ui->scoreEstimateButton->show();
        roundInfoVisible = true;
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
        ui->handicapButton->hide();
        ui->hintButton->show();
        ui->passButton->show();
        ui->undoButton->show();
        whitePlayer->enableChoosingPlayer(false);
        blackPlayer->enableChoosingPlayer(false);
        ui->scoreEstimateButton->show();
        setScoreEstimate(0);
        roundInfoVisible = true;//TODO - get rid of this variable
    }
    else if (state == GameState::Stopped) {
        ui->launchButton->show();
        ui->launchButton->setText("Start");
        ui->finishButton->hide();
        if (roundInfoVisible == true) {
            roundInfo->hide();
            ui->menuLauncher1->hide();
        }
        ui->menuLauncher2->show();
        ui->tableSizeGroupBox->show();
        ui->hintButton->hide();
        ui->passButton->hide();
        ui->undoButton->hide();
        ui->undoButton->setEnabled(true);
        ui->handicapButton->show();
        ui->scoreEstimateButton->hide();
        setShowScoreEstimate(false);
        whitePlayer->enableChoosingPlayer(true);
        blackPlayer->enableChoosingPlayer(true);
        //ui->scoreEstimateButton->hide();
        roundInfoVisible = false;
    }
}

void GameSettings::setScoreEstimate(float score) {
    scoreEstimate = score;
    updateScoreEstimateButton();
}

void GameSettings::updateScoreEstimateButton() {
    QString text;
    if (scoreVisible) {
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

void GameSettings::setCurrentPlayer(int player, PlayerType type, PlayerType opponentType) {
    roundInfo->setCurrentPlayer(player, type, opponentType);
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
    if (scoreVisible)
        setShowScoreEstimate(false);
    else
        setShowScoreEstimate(true);
}

void GameSettings::setShowScoreEstimate(bool show) {
    scoreVisible = show;
    updateScoreEstimateButton();
    emit doEstimateScore(scoreVisible);
}

void GameSettings::showMenu() {
    printf("%s\n", __func__);

    if (false) {
        //typically the menu fonts are too small, so we copy the size of a font know to be decent.
        //this is however ignored by Qt.
        QFont actionFont = mainMenu->actions()[0]->font();
        printf("%s - actionFont: name=%s, pixelSize=%d, pointSize=%d\n", __func__, actionFont.toString().toUtf8().constData(),
               actionFont.pixelSize(), actionFont.pointSize());
        QFont menuFont = mainMenu->font();
        printf("%s - menuFont: name=%s, pixelSize=%d, pointSize=%d\n", __func__, menuFont.toString().toUtf8().constData(),
               menuFont.pixelSize(), menuFont.pointSize());
        QFont buttonFont = ui->launchButton->font();
        printf("%s - buttonFont: name=%s, pixelSize=%d, pointSize=%d\n", __func__,buttonFont.toString().toUtf8().constData(),
               buttonFont.pixelSize(), buttonFont.pointSize());
        mainMenu->setFont(ui->launchButton->font());
        for(int i = 0; i < mainMenu->actions().size(); i++) {
            mainMenu->actions()[i]->setFont(ui->launchButton->font());
            printf("%s - crt action: %s changed to font %s\n", __func__, mainMenu->actions()[i]->text().toUtf8().constData(),
                   mainMenu->actions()[i]->font().toString().toUtf8().constData());
            actionFont = mainMenu->actions()[0]->font();
            printf("%s - actionFont: name=%s, pixelSize=%d, pointSize=%d\n", __func__, actionFont.toString().toUtf8().constData(),
                   actionFont.pixelSize(), actionFont.pointSize());
        }
    }

    mainMenu->show();
    //now which of the the two guys is visible?
    QToolButton* menuLauncher = ui->menuLauncher1;
    if (menuLauncher->isVisible() == false)
        menuLauncher = ui->menuLauncher2;
    QPoint globalPos = menuLauncher->mapToGlobal(QPoint(0, 0));
    globalPos.setX(globalPos.x() - mainMenu->size().width());
    mainMenu->move(globalPos);
}


void GameSettings::showAbout() {
    AboutDialog dialog;
    dialog.exec();
    printf("%s - done\n", __func__);
}

void GameSettings::showHelp() {
    HelpDialog dialog;
    dialog.exec();
}

void GameSettings::showSettings() {
    SettingsDialog dialog;
    dialog.exec();
}

void GameSettings::showHandicapWindow() {
    SGameSettings::Handicap newHandicap = settings.handicap;
    HandicapDialog handicapWindow(newHandicap);
    handicapWindow.setWindowTitle("Handicap");
    int result = handicapWindow.exec();
    printf("%s - result=%d\n", __func__, result);
    if (result == QDialog::Accepted) {
        updateHandicap(newHandicap);
    }
}

void GameSettings::updateHandicap(SGameSettings::Handicap newHandicap) {
    settings.handicap = newHandicap;
    printf("%s - new handicap: komi:%f, stones:%d, placement:%d\n",
           __func__, newHandicap.komi, newHandicap.handicap, newHandicap.handicapPlacementFree);
    QString handicapText = "Komi: ";
    handicapText += QString::number(newHandicap.komi, 'g', 2);
    handicapText += "\nHandicap: ";
    handicapText += QString::number(newHandicap.handicap);
    handicapText += " stones, ";
    if (newHandicap.handicapPlacementFree)
        handicapText += "free";
    else
        handicapText += "fixed";
    ui->handicapButton->setText(handicapText);
}

bool operator==(const SGameSettings::Handicap& h1, const SGameSettings::Handicap& h2) {
    if (h1.komi != h2.komi)
        return false;
    if (h1.handicap != h2.handicap)
        return false;
    if (h1.handicapPlacementFree != h2.handicapPlacementFree)
        return false;
    return true;
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
    if (!(s1.handicap == s2.handicap))
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
    newSettings.handicap = settings.handicap;

    if (newSettings == settings)
        return;
    else {
        printf("%s - settings have changed\n", __func__);
        settings = newSettings;
        emit gameSettingsChanged(settings);
    }
}

void GameSettings::receiveSettings(SGameSettings newSettings) {
    printf("%s\n", __func__);
    switch (newSettings.size) {
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

    blackPlayer->setPlayerType(newSettings.black);
    blackPlayer->setAIStrength(newSettings.blackAIStrength);
    whitePlayer->setPlayerType(newSettings.white);
    whitePlayer->setAIStrength(newSettings.whiteAIStrength);
    updateHandicap(newSettings.handicap);
}

void GameSettings::askConfirmFinishGame() {
    int ret = QMessageBox::question(this, "FreeGo", "Do you want to resign the game?",
                                    QMessageBox::Cancel | QMessageBox::Ok);
    if (ret == QMessageBox::Ok)
        emit finishGamePerform(true);
}

void GameSettings::launchGameClicked() {
    populateSettings();
    emit launchGamePerform(settings);
}

