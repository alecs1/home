#include <QSvgRenderer>
#include <QPainter>

#include "RoundInfo.h"
#include "ui_RoundInfo.h"

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


    printf("%s - default font:%s, %d\n", __func__, defaultFont.toUtf8().constData(), defaultFontSize);
    const float STONE_SCALE= 5;
    float diameter = STONE_SCALE * defaultFontSize;
    ui->colourLabel->resize(diameter, diameter);
    //ui->colourLabel->setMinimumSize(QSize(diameter, diameter));

    setMinimumHeight(diameter);

    printf("%s - colourLabel resize to:%dx%d\n", __func__, ui->colourLabel->width(), ui->colourLabel->height());
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

    //smaller sizes for the player type
    diameter /= 1.5;
    ui->playerTypeLabel->resize(diameter, diameter);
    //ui->playerTypeLabel->setMinimumSize(QSize(diameter, diameter));
    playerAI = new QPixmap(diameter, diameter);
    playerAI->fill(Qt::transparent);
    svgR.load(QString(":/resources/playerAI--picmi.svg"));
    QPainter AIPainter(playerAI);
    svgR.render(&AIPainter);

    playerHuman = new QPixmap(diameter, diameter);
    playerHuman->fill(Qt::transparent);
    svgR.load(QString(":/resources/playerHuman--user-identity.svg"));
    QPainter HumanPainter(playerHuman);
    svgR.render(&HumanPainter);

    playerNetwork = new QPixmap(diameter, diameter);
    playerNetwork->fill(Qt::transparent);
    svgR.load(QString(":/resources/playerNetwork--network-wired.svg"));
    QPainter NetworkPainter(playerNetwork);
    svgR.render(&NetworkPainter);


    setCurrentPlayer(BLACK, PlayerType::LocalHuman);

    printf("%s - final sizes: widget:%dx%d, colourLabel:%dx%d, playerTypeLabel:%dx%d\n",
           __func__, width(), height(), ui->colourLabel->width(), ui->colourLabel->height(),
           ui->playerTypeLabel->width(), ui->playerTypeLabel->height());
}

RoundInfo::~RoundInfo()
{
    delete ui;
}


void RoundInfo::setCurrentPlayer(int aPlayer, PlayerType aType) {
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
    }
    else if (playerType == PlayerType::LocalHuman) {
        crtPlayerPixmap = playerHuman;
        playerTypeString = "Your turn";
    }
    else if (playerType == PlayerType::Network) {
        crtPlayerPixmap = playerNetwork;
        playerTypeString = "Remote player's\n turn";
    }
    ui->playerTypeLabel->setPixmap(*crtPlayerPixmap);
    ui->playerTypeText->setText(playerTypeString);

//    int crtWidth = ui->playerTypeText->width();
//    int preferredHeight = ui->playerTypeText->heightForWidth(crtWidth);
//    setMinimumHeight(preferredHeight + ui->playerTypeLabel->height());
    //printf("%s - pixmap=%p, player=%d\n", __func__, crtPixmap, player);

    update();
}

//TODO - implement later, as it needs math
void RoundInfo::computeAnim(float pos) {


}

//QSize RoundInfo::sizeHint() const {
//    int width = ui->colourLabel->wi
//}
