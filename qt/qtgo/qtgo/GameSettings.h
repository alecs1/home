#ifndef GAMESETTINGS_H
#define GAMESETTINGS_H

#include <QWidget>
#include "Global.h"

class PlayerWidget;

namespace Ui {
class GameSettings;
}

class GameSettings : public QWidget {
Q_OBJECT

public:
    GameSettings(QWidget* parent);

signals:
    void launchGamePerform(SGameSettings settings);
    //void settingsChanged(SGameSettings settings);

public slots:
    void setGameState(GameState state);
    void setScoreEstimate(float score);

private slots:
    void launchGameClicked();

private:
    void populateSettings();

protected:
    //intercept generic events and call populateSettings on each of them
    //void keyReleaseEvent(QKeyEvent * event);
    //void mouseReleaseEvent(QMouseEvent * event);

private:
    Ui::GameSettings* ui;
    SGameSettings settings;
    PlayerWidget* blackPlayer;
    PlayerWidget* whitePlayer;
};


//Show the colour of the current player and maybe some animation, that he is currently "thinking", in case of the computer
class RoundInfo : public QWidget {
public:
    RoundInfo();

private:
    QPixmap* blackStone;
    QPixmap* whiteStone;

};

#endif // GAMESETTINGS_H
