#pragma once

#include <QWidget>

#include "Constants.h"

class QToolButton;
class RoundInfo;
class QVBoxLayout;

//TODO - should this inherit functionality from the larger brother?
class MiniGameControlWidget : public QWidget {
Q_OBJECT
public:
    explicit MiniGameControlWidget(QWidget* parent);
    void addRoundInfo(RoundInfo* aRoundInfo);
    void removeRoundInfo();
public slots:
    void changeProgramSettings();
    void setCurrentPlayer(int player, PlayerType type, PlayerType opponentType);
signals:
    void setFullInterface();
    void userPassedMove();
    void undoMove();
private:
    RoundInfo* roundInfo = NULL;
    QToolButton* passButton = NULL;
    QToolButton* undoButton = NULL;
    QToolButton* fullInterfaceButton = NULL;
    QVBoxLayout* layoutP;
};
