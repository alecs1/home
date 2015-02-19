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
    void setCurrentPlayer(int aPlayer, PlayerType aType);

//    QSize sizeHint() const;


private:
    void computeAnim(float pos);

private:
    Ui::RoundInfo *ui;

    QPixmap* blackStone;
    QPixmap* whiteStone;
    QPixmap* crtStonePixmap;

    QPixmap* playerAI;
    QPixmap* playerHuman;
    QPixmap* playerNetwork;
    QPixmap* crtPlayerPixmap;

    int player;
    PlayerType playerType;
};

#endif // ROUNDINFO_H
