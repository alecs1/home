#include <QSvgRenderer>
#include <QPainter>
#include <QTimer>

#ifdef WithQt5Quick
#include <QtQuickWidgets/QQuickWidget>
#endif

#include "RoundInfo.h"
#include "ui_RoundInfo.h"

#include "Logger.h"

#include "GameStruct.h"


RoundInfo::RoundInfo(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::RoundInfo)
{
    ui->setupUi(this);

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


    Logger::log(QString("%1 - default font:%2, %3").arg(__func__).arg(defaultFont.toUtf8().constData()).arg(defaultFontSize), Logger::DBG);
    const float STONE_SCALE= 5;
    diameter = STONE_SCALE * defaultFontSize;
    ui->colourLabel->resize(diameter, diameter);
    ui->colourLabel->setMinimumSize(QSize(diameter, diameter));

    setMinimumHeight(diameter);

    Logger::log(QString("%1 - colourLabel resize to:%2x%3").arg(__func__).arg(ui->colourLabel->width()).arg(ui->colourLabel->height()), Logger::DBG);
    Logger::log(QString("%1 - size:%2x%3").arg(__func__).arg(width()).arg(height()), Logger::DBG);

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

    //smaller sizes for the player type
    float playerTypeSize = diameter / 1.5;
    ui->playerTypeLabel->resize(playerTypeSize, playerTypeSize);
    playerAI = new QPixmap(playerTypeSize, playerTypeSize);
    playerAI->fill(Qt::transparent);
    svgR.load(QString(":/resources/oxygen-icons/playerAI--picmi.svg"));
    QPainter AIPainter(playerAI);
    svgR.render(&AIPainter);

    playerHuman = new QPixmap(playerTypeSize, playerTypeSize);
    playerHuman->fill(Qt::transparent);
    svgR.load(QString(":/resources/oxygen-icons/playerHuman--user-identity.svg"));
    QPainter HumanPainter(playerHuman);
    svgR.render(&HumanPainter);

    playerNetwork = new QPixmap(playerTypeSize, playerTypeSize);
    playerNetwork->fill(Qt::transparent);
    svgR.load(QString(":/resources/oxygen-icons/playerNetwork--network-wired.svg"));
    QPainter NetworkPainter(playerNetwork);
    svgR.render(&NetworkPainter);

    crtStoneRotated = new QPixmap(diameter, diameter);

    /* TODO:
     * - work with Qt 5.4; - with 5.3 calling show() after hide() "loses the context" and becomes black: https://bugreports.qt.io/browse/QTBUG-41622
     * - disable on Android or invalidate the entire main window; calling show() after hide() hide corrupts the main window
     */
#ifdef WithQt5Quick
    //don't use for now
    if (false) {
        QQuickWidget* animationWidget = new QQuickWidget(this);
        animationWidget->setSource(QUrl("qrc:/RoundInfoAnimation.qml"));
        animationWidget->setResizeMode(QQuickWidget::SizeRootObjectToView);
        //ui->layoutQuick->addWidget(animationWidget, 0, 0);
    }
#endif

    setCurrentPlayer(BLACK, PlayerType::LocalHuman, PlayerType::AI);
    setAttribute(Qt::WA_TranslucentBackground);
    QList<QWidget*> children = findChildren<QWidget*>();
    for(QWidget* child : children) {
        child->setAttribute(Qt::WA_TranslucentBackground);
    }

    Logger::log(QString("%1 - final sizes: widget:%2x%3, colourLabel:%4x%5, playerTypeLabel:%6x%7").arg(__func__).arg(width()).arg(height()).arg(ui->colourLabel->width()).arg(ui->colourLabel->height()).arg(ui->playerTypeLabel->width()).arg(ui->playerTypeLabel->height()), Logger::DBG);

}

RoundInfo::~RoundInfo()
{
    delete ui;
    delete blackStone;
    delete whiteStone;
    delete crtStoneRotated;

    delete playerAI;
    delete playerHuman;
    delete playerNetwork;
}


void RoundInfo::setCurrentPlayer(int aPlayer, PlayerType aType, PlayerType opponentType) {
    Logger::log(QString("%1 - playerType=%2").arg(__func__).arg(playerTypeMap.left.at(aType)), Logger::DBG);
    player = aPlayer;
    playerType = aType;
    if (player == BLACK) {
        crtStonePixmap = blackStone;
    }
    else if (player == WHITE) {
        crtStonePixmap = whiteStone;
    }
    ui->colourLabel->setPixmap(*crtStonePixmap);

    QString playerTypeString;
    if (playerType == PlayerType::AI) {
        crtPlayerPixmap = playerAI;
        playerTypeString = "Computer's\n turn";
        angle = 0;
        Logger::log(QString("%1 - starting animation").arg(__func__), Logger::DBG);
        if (animationChains == 0) {
            animationChains = 1;
            animationStep();
        }
        else {
            //animationChains = 0;
        }
    }
    else if (playerType == PlayerType::LocalHuman) {
        crtPlayerPixmap = playerHuman;
        if (opponentType == PlayerType::LocalHuman) {
            if (player == WHITE)
                playerTypeString = "White's turn";
            else
                playerTypeString = "Black's turn";
        }
        else {
            playerTypeString = "Your turn";
        }
    }
    else if (playerType == PlayerType::Network) {
        crtPlayerPixmap = playerNetwork;
        playerTypeString = "Remote player's\n turn";
    }
    ui->playerTypeLabel->setPixmap(*crtPlayerPixmap);
    ui->playerTypeText->setText(playerTypeString);

    update();
}

void RoundInfo::setLayoutDirection(bool horizontal) {
    if (horizLayout == horizontal)
        return;
    horizLayout = horizontal;
    //Resizing before laying out to make animations look nice
    QRect auxSize = ui->playerTypeLayout->geometry();
    ui->gridLayout->removeItem(ui->playerTypeLayout);
    if (horizontal) {
        ui->gridLayout->addLayout(ui->playerTypeLayout, 0, 1);
        resize(width() + auxSize.width(), height() - auxSize.height());
    }
    else {
        ui->gridLayout->addLayout(ui->playerTypeLayout, 1, 0);
        resize(width() - auxSize.width(), height() + auxSize.height());
    }
}

void RoundInfo::animationStep() {
    //printf("%s - angle=%g, animationChains=%d\n", __func__, angle, animationChains);
    if (playerType != PlayerType::AI) {
        animationChains -= 1;
        return;
    }

    crtStoneRotated->fill(Qt::transparent);
    QPainter painter(crtStoneRotated);
    painter.translate(diameter/2, diameter/2);
    painter.rotate(angle);
    painter.drawPixmap(-diameter/2, -diameter/2, diameter, diameter, *crtStonePixmap);
    ui->colourLabel->setPixmap(*crtStoneRotated);

    angle += rotationPerPeriod;
    QTimer::singleShot(rotationPeriod, this, SLOT(animationStep()));
}
