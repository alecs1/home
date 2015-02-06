#ifndef GAMESETTINGS_H
#define GAMESETTINGS_H

#include <QWidget>
#include "Global.h"

class PlayerWidget;
class RoundInfo;
class ConfirmMoveDialog;

namespace Ui {
class GameSettings;
}

class GameSettings : public QWidget {
Q_OBJECT

public:
    GameSettings(QWidget* parent);
    ~GameSettings();

signals:
    void launchGamePerform(SGameSettings settings);
    void doEstimateScore(bool estimate);
    void userConfirmedMove(int confirmed);

public slots:
    void setGameState(GameState state);
    void setScoreEstimate(float score);
    void setCurrentPlayer(int player, PlayerType type);
    void showConfirmButton(bool show);
    void toggleShowEstimateScore();

private slots:
    void launchGameClicked();

private:
    void populateSettings();
    void updateScoreEstimateButton();

protected:
    //intercept generic events and call populateSettings on each of them
    //void keyReleaseEvent(QKeyEvent * event);
    //void mouseReleaseEvent(QMouseEvent * event);

private:
    Ui::GameSettings* ui;
    SGameSettings settings;
    PlayerWidget* blackPlayer;
    PlayerWidget* whitePlayer;
    bool showingRoundInfo;
    RoundInfo* roundInfo;
    bool showScore;
    float scoreEstimate;
    ConfirmMoveDialog* confirmMoveDialog;

};


//Show the colour of the current player and maybe some animation, that he is currently "thinking", in case of the computer
class RoundInfo : public QWidget {
//Q_OBJECT
public:
    RoundInfo(QWidget* parent);
    void paintEvent(QPaintEvent *);
    void setCurrentPlayer(int aPlayer, PlayerType aType);

private:
    void computeAnim(float pos);

private:
    QPixmap* blackStone;
    QPixmap* whiteStone;
    QPixmap* crtPixmap;
    int player;
    PlayerType playerType;
};

#endif // GAMESETTINGS_H
