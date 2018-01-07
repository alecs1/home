#include "GameInvitation.h"
#include "ui_GameInvitation.h"

#include <QSvgRenderer>
#include <QPainter>

#include "../Logger.h"

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

void GameInvitation::setGridImageWidget(QWidget* widget) {
    QVBoxLayout* aux = dynamic_cast<QVBoxLayout*>(layout());
    aux->insertWidget(0, widget);
}

void GameInvitation::setColour(const colors colour) {
    int diameter = 100;

    QString resourceStr(":/resources/cursorWhite.svg");

    if (colour == BLACK) {
        resourceStr = ":/resources/cursorBlack.svg";
    }
    else if (colour != WHITE) {
        Logger::log(QString("Invalid colour %1").arg(colour));
        resourceStr = ":/resources/cursorRed.svg";
    }

    QSvgRenderer svgR;
    svgR.load(resourceStr);
    QPixmap stoneColourPixmap = QPixmap(diameter, diameter);
    stoneColourPixmap.fill(Qt::transparent);
    QPainter bPainter(&stoneColourPixmap);
    svgR.render(&bPainter);

    ui->colourLabelImage->setPixmap(stoneColourPixmap);
}
