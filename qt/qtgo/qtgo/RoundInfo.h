#ifndef ROUNDINFO_H
#define ROUNDINFO_H

#include <QWidget>

#include "Global.h"

namespace Ui {
class RoundInfo;
}

class RoundInfo : public QWidget
{
    Q_OBJECT

public:
    explicit RoundInfo(QWidget *parent = 0);
    ~RoundInfo();
    //void paintEvent(QPaintEvent *);
    void setCurrentPlayer(int aPlayer, PlayerType aType, PlayerType opponentType);

public slots:
    void animationStep();

signals:
    void nextAnimationStep(double);

private:
    Ui::RoundInfo *ui;

    QPixmap* blackStone;
    QPixmap* whiteStone;
    QPixmap* crtStonePixmap;
    QPixmap* crtStoneRotated;

    QPixmap* playerAI;
    QPixmap* playerHuman;
    QPixmap* playerNetwork;
    QPixmap* crtPlayerPixmap;

    int player;
    PlayerType playerType;

    double diameter;
    const int rotationPeriod = 30;
    const float rotationPerPeriod = 5;
    double angle = 0;
    int animationChains = 0;
};

#endif // ROUNDINFO_H
