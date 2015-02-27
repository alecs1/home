#ifndef GAMESETTINGS_H
#define GAMESETTINGS_H

#include <QWidget>
#include "Global.h"

class PlayerWidget;
class RoundInfo;
class ConfirmMoveDialog;
class QMenu;
class QToolButton;

namespace Ui {
class GameSettings;
}

class GameSettings : public QWidget {
Q_OBJECT

public:
    GameSettings(QWidget* parent);
    ~GameSettings();

signals:
    //TODO - having two functions with SGameSettigns seems like duplicating
    void launchGamePerform(SGameSettings settings);
    void finishGamePerform();
    void doEstimateScore(bool estimate);
    void userConfirmedMove(int confirmed);
    void userPassedMove();
    void gameSettingsChanged(SGameSettings settings);


public slots:
    void setGameState(GameState state);
    void setScoreEstimate(float score);
    void setCurrentPlayer(int player, PlayerType type);
    void showConfirmButton(bool show);
    void toggleShowEstimateScore();
    void showMenu();
    void receiveSettings(SGameSettings settings);
    void askConfirmFinishGame();

private slots:
    void launchGameClicked();
    void populateSettings();

private:
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
    GameState gameState;
    ConfirmMoveDialog* confirmMoveDialog;
    QMenu* mainMenu;
};


#endif // GAMESETTINGS_H
