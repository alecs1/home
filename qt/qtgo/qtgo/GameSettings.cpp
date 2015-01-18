#include <QSvgRenderer>
#include <QPainter>

#include "GameSettings.h"
#include "ui_GameSettings.h"

#include "PlayerWidget.h"

GameSettings::GameSettings(QWidget *parent):
    ui(new Ui::GameSettings())
{
    ui->setupUi(this);

    connect(ui->launchButton, SIGNAL(clicked()), this, SLOT(launchGameClicked()));

    blackPlayer = new PlayerWidget(this);
    ui->gridLayout->addWidget(blackPlayer, 3, 0);
    blackPlayer->setPlayerType(settings.black);

    whitePlayer = new PlayerWidget(this);
    whitePlayer->setTitle("White");
    ui->gridLayout->addWidget(whitePlayer, 4, 0);
    whitePlayer->setPlayerType(settings.white);

    populateSettings();

    roundInfo = new RoundInfo(ui->roundInfoWidget);
    ui->roundInfoWidget->setMinimumHeight(roundInfo->height());
    showingRoundInfo = false;
    ui->roundInfoWidget->hide();

    printf("%s - roundInfoWidget size:%dx%d\n", __func__, ui->roundInfoWidget->width(), ui->roundInfoWidget->height());
}

void GameSettings::setGameState(GameState state) {
    if (state == GameState::Started) {
        ui->launchButton->setText("Finish");
        ui->tableSizeGroupBox->setEnabled(false);
        ui->roundInfoWidget->show();
        showingRoundInfo = true;
        printf("%s - roundInfoWidget size:%dx%d\n", __func__, ui->roundInfoWidget->width(), ui->roundInfoWidget->height());
    }
    else if (state == GameState::Stopped) {
        ui->launchButton->setText("Start");
        ui->tableSizeGroupBox->setEnabled(true);
        if (showingRoundInfo == true) {
            ui->roundInfoWidget->hide();
            showingRoundInfo = false;
            printf("%s - roundInfoWidget size:%dx%d\n", __func__, ui->roundInfoWidget->width(), ui->roundInfoWidget->height());
        }
    }
}

void GameSettings::setScoreEstimate(float score) {
    QString text;
    text.sprintf("Est: %3.1f", score);
    ui->scoreEstimateLabel->setText(text);
}

bool operator==(const SGameSettings& s1, const SGameSettings& s2) {
    if (s1.AILevel != s2.AILevel)
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
    newSettings.white = (PlayerType)whitePlayer->playerType();

    if (newSettings == settings)
        return;
    else {
        printf("%s - settings have changed\n", __func__);
        settings = newSettings;
        //emit settingsChanged(settings);
    }
}

void GameSettings::launchGameClicked() {
    populateSettings();
    emit launchGamePerform(settings);
}

/*
void GameSettings::keyReleaseEvent(QKeyEvent * event) {
    populateSettings();
    QWidget::keyReleaseEvent(event);
}

void GameSettings::mouseReleaseEvent(QMouseEvent * event) {
    populateSettings();
    QWidget::mouseReleaseEvent(event);
}
*/

RoundInfo::RoundInfo(QWidget* parent) :
    QWidget(parent)
{
    QSvgRenderer svgR;

    //this is a fixed size during gameplay, but yet computed at program start-up.
    //find the system font size and make the drawing a couple of times larger

    QFont font;
    QString defaultFont = font.defaultFamily();
    int defaultFontSize = font.pointSize();

    printf("%s - default font:%s, %d\n", __func__, defaultFont.toUtf8().constData(), defaultFontSize);
    const int SCALE= 4;
    int diameter = SCALE * defaultFontSize;
    resize(diameter, diameter);

    printf("%s - size:%dx%d\n", __func__, width(), height());

    blackStone = new QPixmap(diameter, diameter);
    blackStone->fill(Qt::transparent);
    svgR.load(QString(":/resources/cursorBlack.svg"));
    QPainter bPainter(blackStone);
    svgR.render(&bPainter);
}

void RoundInfo::paintEvent(QPaintEvent *) {
    QPainter painter(this);
    painter.drawPixmap(0, 0, blackStone->width(), blackStone->height(), *blackStone);
    //render(blackStone);
}
