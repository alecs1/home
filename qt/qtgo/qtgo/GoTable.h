#ifndef GOTABLE_H
#define GOTABLE_H

#include <QWidget>
#include <QThread>

#include "Global.h"

class GoTable;
class AIThread : public QThread {
Q_OBJECT
    void run() override;

public:
    bool run_do_genmove(int color, float pure_threat_value, int* allowed_moves);

signals:
    void AIThreadPlaceStone(int row, int col);

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
    bool placeStone(int row, int col);
    //void settingsChanged(SGameSettings newSettings);

private slots:
    bool AIPlayNextMove();

signals:
    void gameStateChanged(GameState state);
    void estimateScoreChanged(float score);
    void crtPlayerChanged(int player, PlayerType type);

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
    float dist;
    int highlightRow;
    int highlightCol;
    int newStoneRow;
    int newStoneCol;
    GameState state = GameState::Stopped;

    //populate this with some default settings, which are then passed to the game
    SGameSettings settings;

    int crtPlayer;
    PlayerType players[3]; //board.h enum: EMPTY, WHITE, BLACK

    /*we need to block input on the widget, with this hackish way:
    time difference between the click that triggered blocking and current click must be bigger than the amount of time we decided to block input:
    _click1_        _click2_(ignored)                           _click1_(accepted)
                _block_input    _compute_   _unblock_input_
    */
    ulong lastInputTimestamp;
    ulong inputBlockingDuration = 0;
    QTime* blockTime;
    bool cursorBlocked;

    bool useGNUGO = true;

    AIThread aiThread;

private:
    bool buildPixmaps(int diameter);
    void updateCursor();

    void resetGnuGo();
    int toGnuGoPos(int row, int col);
    void printfGnuGoStruct();
    bool isValidPos(int row, int col); //need extra checks, because is_valid() from GnuGo actually uses fucking asserts
    int populateStructFromGnuGo(); //populate our own structure from GnuGo; this will keep to a minimum places where the useGNUGO is used
    void updateSizes();

    bool shouldRejectInput(QMouseEvent *ev);


    void launchGame();
};


#endif // GOTABLE_H
