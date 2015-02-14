#ifndef GOTABLE_H
#define GOTABLE_H

#include <QWidget>
#include <QThread>

#include "Global.h"

class QMutex;

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
    bool run_do_genmove(int color, float pure_threat_value, int* allowed_moves);

signals:
    void AIThreadPlaceStone(int row, int col);
    void AIQuitsGame();

private:
    bool running = false;
    struct Parameters {
        int color;
        float pure_threat_value;
        int* allowed_moves;
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

public slots:
    void launchGamePressed(SGameSettings newSettings);
    void changeGameSettings(SGameSettings newSettings);
    bool placeStone(int row, int col);
    bool passMove();
    void finish();
    void activateEstimatingScore(bool estimate);
    void userConfirmedMove(int confirmed);

private slots:
    bool AIPlayNextMove();

signals:
    void gameStateChanged(GameState state);
    void estimateScoreChanged(float score);
    void crtPlayerChanged(int player, PlayerType type);
    void askUserConfirmation(bool ask); //ask the user for confirmation, dialog belongs to another widget

protected:
    void resizeEvent(QResizeEvent *event);

    void mouseMoveEvent(QMouseEvent *ev);
    void mousePressEvent(QMouseEvent* ev);
    void mouseReleaseEvent(QMouseEvent* ev);

    //use QPoint as tuple of coordinates
    QPoint mouseToGameCoordinates(QMouseEvent* ev);

private:
    QCursor* blackCursor;
    QCursor* whiteCursor;
    QCursor* redCursor;
    QPixmap* blackStonePixmap;
    QPixmap* whiteStonePixmap;
    QPixmap* redStonePixmap;
    float dist; //distance between two table lines
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

    bool askPlayConfirmation; //ask the user to confirm placement of a stone;
    bool acceptDoubleClickConfirmation = false;
    GameState state = GameState::Stopped;

    //populate this with some default settings, which are then passed to the game
    SGameSettings settings;

    int crtPlayer = 1;
    PlayerType players[3]; //board.h enum: EMPTY, WHITE, BLACK

    /*we need to block input on the widget, with this hackish way:
    time difference between the click that triggered blocking and current click must be bigger than the amount of time we decided to block input:
    _click1_        _click2_(ignored)                           _click1_(accepted)
                _block_input    _compute_   _unblock_input_
    */
    ulong lastInputTimestamp = 0;
    ulong inputBlockingDuration = 0;
    QTime* blockTime = NULL;
    bool cursorBlocked = false;

    bool useGNUGO = true;
    bool estimateScore = false;
    QMutex* gnuGoMutex = NULL;

    AIThread* aiThread = NULL;
    ElapsedTimerWrapper timer;
    QString timerDelta;

private:
    bool buildPixmaps(int diameter);
    void updateCursor();

    void resetGnuGo();
    int toGnuGoPos(int row, int col);
    void printfGnuGoStruct();
    bool isPosInsideTable(int row, int col); //need extra checks, because is_valid() from GnuGo actually uses fucking asserts
    int populateStructFromGnuGo(); //populate our own structure from GnuGo; this will keep to a minimum places where the useGNUGO is used
    void updateSizes();
    bool shouldRejectInput(QMouseEvent *ev);
    void launchGame(bool resetTable = true);
};


#endif // GOTABLE_H
