#include <QSvgRenderer>
#include <QPainter>

#include "GameSettings.h"
#include "ui_GameSettings.h"

#include "PlayerWidget.h"
#include "GameStruct.h"
#include "ConfirmMoveDialog.h"

GameSettings::GameSettings(QWidget *parent):
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

    connect(ui->launchButton, SIGNAL(clicked()), this, SLOT(launchGameClicked()));
    connect(ui->scoreEstimateButton, SIGNAL(clicked()), this, SLOT(toggleShowEstimateScore()));

    connect(blackPlayer, SIGNAL(playerTypeChanged(int)), this, SLOT(populateSettings()));
    connect(whitePlayer, SIGNAL(playerTypeChanged(int)), this, SLOT(populateSettings()));
    connect(blackPlayer, SIGNAL(playerStrengthChanged(int)), this, SLOT(populateSettings()));
    connect(whitePlayer, SIGNAL(playerStrengthChanged(int)), this, SLOT(populateSettings()));
    connect(ui->button9x9, SIGNAL(toggled(bool)), this, SLOT(populateSettings()));
    connect(ui->button13x13, SIGNAL(toggled(bool)), this, SLOT(populateSettings()));
    connect(ui->button19x19, SIGNAL(toggled(bool)), this, SLOT(populateSettings()));
    connect(ui->passButton, SIGNAL(clicked()), this, SIGNAL(userPassedMove()));

    populateSettings();

    roundInfo = new RoundInfo(ui->roundInfoWidget);
    ui->roundInfoWidget->setMinimumHeight(roundInfo->height());
    showingRoundInfo = false;
    ui->roundInfoWidget->hide();

    ui->hintButton->hide();
    ui->passButton->hide();

    showScore = false;
    setGameState(GameState::Initial);

    confirmMoveDialog = NULL;

    printf("%s - roundInfoWidget size:%dx%d\n", __func__, ui->roundInfoWidget->width(), ui->roundInfoWidget->height());
    printf("%s - end\n", __func__);
}

GameSettings::~GameSettings() {
    delete confirmMoveDialog;
}

void GameSettings::setGameState(GameState state) {
    gameState = state;
    if (state == GameState::Initial) {
        ui->scoreEstimateButton->hide();
        setScoreEstimate(0);
    }
    if (state == GameState::Started) {
        ui->launchButton->setText("Finish");
        ui->tableSizeGroupBox->setEnabled(false);
        ui->roundInfoWidget->show();
        ui->tableSizeGroupBox->hide();
        ui->hintButton->show();
        ui->passButton->show();
        whitePlayer->enableChoosingPlayer(false);
        blackPlayer->enableChoosingPlayer(false);
        ui->scoreEstimateButton->show();
        setScoreEstimate(0);
        showingRoundInfo = true;
        //printf("%s - roundInfoWidget size:%dx%d\n", __func__, ui->roundInfoWidget->width(), ui->roundInfoWidget->height());
    }
    else if (state == GameState::Stopped) {
        ui->launchButton->setText("Start");
        ui->tableSizeGroupBox->setEnabled(true);
        if (showingRoundInfo == true) {
            ui->roundInfoWidget->hide();
        }
        ui->tableSizeGroupBox->show();
        ui->hintButton->hide();
        ui->passButton->hide();
        whitePlayer->enableChoosingPlayer(true);
        blackPlayer->enableChoosingPlayer(true);
        //ui->scoreEstimateButton->hide();
        showingRoundInfo = false;
        //printf("%s - roundInfoWidget size:%dx%d\n", __func__, ui->roundInfoWidget->width(), ui->roundInfoWidget->height());
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
}

void GameSettings::showConfirmButton(bool show) {
    if (show == false) {
        if (confirmMoveDialog != NULL) {
            confirmMoveDialog->hide();
        }
        return;
    }
    if ((gameState != GameState::Started) && (gameState != GameState::Initial))
        return;

    if (confirmMoveDialog == NULL) {
        confirmMoveDialog = new ConfirmMoveDialog(this);
        connect(confirmMoveDialog, SIGNAL(finished(int)), this, SIGNAL(userConfirmedMove(int)));
    }
    //show dialog over the settings UI, but somehow seems hackish; just replace instead of creating a new window.
    QPoint globalPos = mapToGlobal(QPoint(0, 0));
    confirmMoveDialog->setGeometry(QRect(globalPos, this->size()));
    confirmMoveDialog->setWindowFlags(Qt::FramelessWindowHint | confirmMoveDialog->windowFlags());
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

void GameSettings::launchGameClicked() {
    populateSettings();
    emit launchGamePerform(settings);
}

RoundInfo::RoundInfo(QWidget* parent) :
    QWidget(parent)
{
    QSvgRenderer svgR;

    //this is a fixed size during gameplay, but yet computed at program start-up.
    //find the system font size and make the drawing a couple of times larger

    QFont font;
    QString defaultFont = font.defaultFamily();
    int defaultFontSize = font.pixelSize();
    if (defaultFontSize <= 0)
        defaultFontSize = font.pointSize();
    if (defaultFontSize <= 0) {
        printf("%s - error - could not establish a fonst size!\n", __func__);
    }



    printf("%s - default font:%s, %d\n", __func__, defaultFont.toUtf8().constData(), defaultFontSize);
    const int SCALE= 5;
    int diameter = SCALE * defaultFontSize;
    resize(diameter, diameter);

    printf("%s - size:%dx%d\n", __func__, width(), height());

    blackStone = new QPixmap(diameter, diameter);
    blackStone->fill(Qt::transparent);
    svgR.load(QString(":/resources/cursorBlack.svg"));
    QPainter bPainter(blackStone);
    svgR.render(&bPainter);

    whiteStone = new QPixmap(diameter, diameter);
    whiteStone->fill(Qt::transparent);
    svgR.load(QString(":/resources/cursorWhite.svg"));
    QPainter wPainter(whiteStone);
    svgR.render(&wPainter);

    crtPixmap = blackStone;
}

void RoundInfo::paintEvent(QPaintEvent *) {
    //printf("%s - pixmap=%p, player=%d\n", __func__, crtPixmap, player);
    QPainter painter(this);
    painter.drawPixmap(0, 0, crtPixmap->width(), crtPixmap->height(), *crtPixmap);
}

void RoundInfo::setCurrentPlayer(int aPlayer, PlayerType aType) {
    player = aPlayer;
    playerType = aType;
    if (player == BLACK) {
        crtPixmap = blackStone;
    }
    else if (player == WHITE) {
        crtPixmap = whiteStone;
    }
    //printf("%s - pixmap=%p, player=%d\n", __func__, crtPixmap, player);
    update();
}

//TODO - implement later, as it needs math
void RoundInfo::computeAnim(float pos) {


}
