#ifndef GAMESETTINGS_H
#define GAMESETTINGS_H

#include <QWidget>
#include "Global.h"

class PlayerWidget;
class RoundInfo;
class ConfirmMoveDialog;
class QMenu;
class QToolButton;
class QAction;

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
    void undoMove();
    void showHints();
    void gameSettingsChanged(SGameSettings settings);
    void saveGame();
    void loadGame();


public slots:
    void setGameState(GameState state);
    void setScoreEstimate(float score);
    void setCurrentPlayer(int player, PlayerType type);
    void showConfirmButton(bool show, int colour);
    void toggleShowEstimateScore();
    void setShowScoreEstimate(bool show);
    void showMenu();
    void showAbout();
    void showDebugWindow();
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

    QAction* saveGameAction;
    QAction* loadGameAction;
    QAction* aboutAction;
    QAction* debugAction;
};


#endif // GAMESETTINGS_H
