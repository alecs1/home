#pragma once

#include <QObject>

#include "Global.h"
#include "GameStruct.h"
#include "Settings.h"

#include <sgf/sgftree.h>

struct board_lib_state_struct;

class AIThread;
class QMutex;

class GoTable : public QObject {
Q_OBJECT
public:
    explicit GoTable();
    ~GoTable();

    void checkForResumeGame();

    //direction: 0 - width, 1 - height
    bool saveGame(QJsonObject& json);
    bool saveGame(QString fileName);
    bool saveGameForRemote(QJsonObject& json);
    bool loadGame(SGFNode *aux, SGameSettings auxSettings, SAuxGameInfo auxGameInfo);
    bool loadGame(const QString fileName);
    bool loadGameFromRemote(const QJsonObject& json);

    bool loadGameAndStart(const QString fileName);

    GameState getGameState() const;
    void getPlayersState(int& crt, PlayerType& crtType, int &opponent, PlayerType& opponentType) const;

    void setSecondPlayerToNetwork();

    //TODO - temporary hack
    SGameSettings* getGameSettingsPointer();


public:
    static QPoint fromGnuGoPos(int pos);
    static int toGnuGoPos(int row, int col);


signals:
    void gameStateChanged(GameState state);
    void movePlayed(int row, int col);
    void crtPlayerChanged(int player, PlayerType type, PlayerType oponentType);

public:
    void changeGameSettings(const SGameSettings &newSettings);
    void changeProgramSettings();
    bool playMove(const int row, const int col);
    bool passMove();
    bool undoMove();
    float finish(bool finishByResign);
    void activateEstimatingScore(bool estimate);
    int insertDefaultHandicap(int handicap);
    float wrapper_gnugo_estimate_score(float* upper, float* lower, bool waitForLock = true, bool *success = nullptr);
    float wrapper_aftermath_compute_score();
    void launchGame(bool resetTable = true);
    bool AIPlayNextMove();
    void updateLogic();

protected:
    bool moveIsLegal(int row, int col, int colour); //need extra checks, because is_valid() from GnuGo actually uses fucking asserts
    bool loadSaveGameFile(QString fileName);

    void resetGnuGo(int newSize);
    void printfGnuGoStruct();
    int populateStructFromGnuGo(); //populate our own structure from GnuGo; this will keep to a minimum places where the useGNUGO is used
    void replay_node(SGFNode *node, int color_to_replay, float *replay_score,
                     float *total_score, int* playedMoves, int* crtColour, SGFTree* outTree);

public:
    PlayerType players[3]; //board.h enum: EMPTY, WHITE, BLACK
    int crtPlayer = 1;
    GameStruct game;
    //-1 -> not showing; -2 -> passed; TODO - also show passes
    int lastMoveRow = -1;
    int lastMoveCol = -1;
    GameState state = GameState::Stopped;
    //TODO - game and settings size duplicate info!
    //populate this with some default settings, which are then passed to the game
    SGameSettings gameSettings;
    //TODO - see if we should expose this
    AIThread* aiThread = nullptr;

    board_lib_state_struct* internal_state = nullptr;

protected:
    int passCount = 0;


private:
    //GNUGo related:
    bool useGNUGO = true;
    bool estimateScore = false;
    QMutex* gnuGoMutex = nullptr;
    bool computing = false;
    SGFTree* sgfTree;
    QString crtGameSfgFName = "FreeGoSave.autosave";

    //game save related
    SAuxGameInfo auxInfo;
};
