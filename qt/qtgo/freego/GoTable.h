#ifndef GOTABLE_H
#define GOTABLE_H

#include <QWidget>
#include <QThread>

#include "Global.h"
#include "GameStruct.h"
#include "Settings.h"

#include <sgf/sgftree.h>

class QMutex;
//class SaveFile;

//Very basic wrapper until (if) I decide for a nice way to get timestamps
class QElapsedTimer;
class ElapsedTimerWrapper {
    QElapsedTimer* t;
    uint64_t lastTimestamp;
public:
    ElapsedTimerWrapper();
    ~ElapsedTimerWrapper();

    uint64_t getTimestamp(uint64_t* delta = NULL);
    QString getTimestampStr(QString* delta = NULL);
    uint64_t getElapsed();
    QString getElapsedStr();
};

class AIThread : public QThread {
Q_OBJECT
    void run() override;

public:
    AIThread(QMutex* mutex);
    bool run_genmove(int color, int AIStrength);
    bool run_gnugo_estimate_score();
    void run_value_moves(int colour);

signals:
    void AIThreadPlaceStone(int row, int col);
    void AIQuitsGame(bool accurate);

private:
    bool running = false;
    enum class OpType:uint8_t {
        //keep the operation names in sync with the corresponding functions
        do_genmove = 1,
        gnugo_estimate_score
    };

    struct Parameters {
        OpType operation;

        //params for do_genmove
        int strength;
        int color;
        float move_value;
        float value;
        int resign;
        int result;
    };
    Parameters p;
    QMutex* mutex = NULL;
};


class GoTable : public QWidget {
Q_OBJECT
public:
    explicit GoTable(QWidget* parent = 0);
    ~GoTable();

    void paintEvent(QPaintEvent *);
    static QPoint fromGnuGoPos(int pos);
    static float gridDist(float tableSize, int gameSize);
    static float stoneDiameter();
    int toGnuGoPos(int row, int col);
    void checkForResumeGame();

    //direction: 0 - width, 1 - height
    bool saveGame(QString fileName);
    bool loadGame(QString fileName);
    QString getFullGame() const;

    GameState getGameState() const;


    //TODO - temporary hack
    SGameSettings* getGameSettingsPointer();


public slots:
    void launchGamePressed(SGameSettings newSettings);
    void changeGameSettings(SGameSettings newSettings);
    void changeProgramSettings();
    bool playMove(int row, int col);
    bool passMove();
    bool undoMove();
    void finish(bool finishByResign);
    void activateEstimatingScore(bool estimate);
    void userConfirmedMove(int confirmed);
    void showPlayHints();
    void insertDefaultHandicap(int handicap);

private slots:
    bool AIPlayNextMove();
    void computeScoreAndUpdate();


signals:
    void gameStateChanged(GameState state);
    void estimateScoreChanged(float score);
    void crtPlayerChanged(int player, PlayerType type, PlayerType oponentType);
    void askUserConfirmation(bool ask, int colour=EMPTY); //ask the user for confirmation, dialog belongs to another widget
    void pushGameSettings(SGameSettings newSettings);
    void highlightChanged(int col, int row);

protected:
    void resizeEvent(QResizeEvent *event);

    void mouseMoveEvent(QMouseEvent *ev);
    void mousePressEvent(QMouseEvent* ev);
    void mouseReleaseEvent(QMouseEvent* ev);

    //use QPoint as tuple of coordinates
    QPoint mouseToGameCoordinates(QMouseEvent* ev);

private:
    bool buildPixmaps(int diameter);
    void updateCursor();
    bool moveIsLegal(int row, int col, int colour); //need extra checks, because is_valid() from GnuGo actually uses fucking asserts
    void updateSizes();
    bool shouldRejectInput(QMouseEvent *ev);
    void launchGame(bool resetTable = true);
    bool loadSaveGameFile(QString fileName);

    float wrapper_gnugo_estimate_score(float* upper, float* lower, bool waitForLock = true, bool *success = NULL);
    void resetGnuGo(int newSize);
    void printfGnuGoStruct();
    int populateStructFromGnuGo(); //populate our own structure from GnuGo; this will keep to a minimum places where the useGNUGO is used
    void replay_node(SGFNode *node, int color_to_replay, float *replay_score,
                     float *total_score, int* playedMoves, int* crtColour, SGFTree* outTree);

private:
    QCursor* blackCursor;
    QCursor* whiteCursor;
    QCursor* redCursor;
    QPixmap* blackStonePixmap;
    QPixmap* whiteStonePixmap;
    QPixmap* redStonePixmap;
    float dist; //distance between two table lines
    float diameter;
    int highlightRow;
    int highlightCol;
    //position of the new stone between mouse press and mouse release
    int newStoneRow;
    int newStoneCol;
    //position of the new stone when game asks for confirmation
    int unconfirmedStoneRow;
    int unconfirmedStoneCol;

    //-1 -> not showing; -2 -> passed; TODO - also show passes
    int lastMoveRow = -1;
    int lastMoveCol = -1;

    bool showHints = false;

    bool askPlayConfirmation; //ask the user to confirm placement of a stone;
    bool acceptDoubleClickConfirmation = false;
    GameState state = GameState::Stopped;

    //populate this with some default settings, which are then passed to the game
    SGameSettings gameSettings;
    SProgramSettings* programSettings;

    //TODO - game and settings size duplicate info!
    GameStruct game;
    int crtPlayer = 1;
    PlayerType players[3]; //board.h enum: EMPTY, WHITE, BLACK
    int passCount = 0;

    /*we need to block input on the widget, with this hackish way:
    time difference between the click that triggered blocking and current click must be bigger than the amount of time we decided to block input:
    _click1_        _click2_(ignored)                           _click1_(accepted)
                _block_input    _compute_   _unblock_input_
    */
    ulong lastInputTimestamp = 0;
    ulong inputBlockingDuration = 0;
    QTime* blockTime = NULL;
    bool cursorBlocked = false;

    //GNUGo related:
    bool useGNUGO = true;
    bool estimateScore = false;
    QMutex* gnuGoMutex = NULL;
    AIThread* aiThread = NULL;
    bool computing = false;
    SGFTree* sgfTree;
    QString crtGameSfgFName = "FreeGoSave.autosave";

    //game save related
    SAuxGameInfo auxInfo;

    ElapsedTimerWrapper timer;
    QString timerDelta;

};


#endif // GOTABLE_H
