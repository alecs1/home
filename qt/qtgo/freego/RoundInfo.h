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
    void setCurrentPlayer(int aPlayer, PlayerType aType, PlayerType opponentType);
    void setLayoutDirection(bool horizontal);

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

    //NOTE: keep this in sync when changing the ui file.
    //QList<QWidget*> widgets;

    bool horizLayout = true;
    int player;
    PlayerType playerType;

    double diameter;
    const int rotationPeriod = 30;
    const float rotationPerPeriod = 5;
    double angle = 0;
    int animationChains = 0;
};

#endif // ROUNDINFO_H
