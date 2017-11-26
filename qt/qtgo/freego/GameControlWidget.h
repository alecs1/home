#pragma once

/**
 * The right hand widget which shows the game settings.
 */

#include <QWidget>
#include "Global.h"
#include "Settings.h"

class PlayerWidget;
class RoundInfo;
class ConfirmMoveDialog;
class QMenu;
class QAction;

namespace Ui {
class GameControlWidget;
}

class GameControlWidget : public QWidget {
Q_OBJECT

public:
    explicit GameControlWidget(QWidget* parent);
    ~GameControlWidget();
    RoundInfo* popRoundInfo();
    void pushBackRoundInfo();
    void setActions(QList<QAction *> &actions);

signals:
    //TODO - having two functions with SGameSettigns seems like duplicating
    void launchGamePerform(SGameSettings settings);
    void resign();
    void doEstimateScore(bool estimate);
    void userConfirmedMove(int confirmed);
    void userPassedMove();
    void undoMove();
    void showHints();
    void gameSettingsChanged(SGameSettings settings);


public slots:
    void setGameState(GameState state);
    void setScoreEstimate(float score);
    void setCurrentPlayer(int player, PlayerType type, PlayerType opponentType);
    void showConfirmButton(bool show, int colour);
    void toggleShowEstimateScore();
    void setShowScoreEstimate(bool show);
    void showMenu();
    void showHandicapWindow();
    void updateHandicap(SGameSettings::Handicap newHandicap);
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
    Ui::GameControlWidget* ui;
    SGameSettings settings;
    PlayerWidget* blackPlayer = nullptr;
    PlayerWidget* whitePlayer = nullptr;
    bool roundInfoVisible;
    RoundInfo* roundInfo = nullptr;
    bool scoreVisible;
    float scoreEstimate;
    GameState gameState;
    ConfirmMoveDialog* confirmMoveDialog;
    QMenu* mainMenu = nullptr;
};
