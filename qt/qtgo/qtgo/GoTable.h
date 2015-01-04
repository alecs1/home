#ifndef GOTABLE_H
#define GOTABLE_H

#include <QWidget>

#include "Global.h"


class GoTable : public QWidget {
Q_OBJECT
public:
    explicit GoTable(QWidget* parent = 0);
    ~GoTable();

    void paintEvent(QPaintEvent *);

public slots:
    void launchGamePressed(SGameSettings newSettings);
    //void settingsChanged(SGameSettings newSettings);

private slots:
    bool AIPlayNextMove();

signals:
    void GameStateChanged(GameState state);

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

    bool useGNUGO = true;

private:
    bool buildPixmaps(int diameter);
    void updateCursor();
    bool placeStone(int row, int col);

    void resetGnuGo();
    int toGnuGoPos(int row, int col);
    QPoint fromGnuGoPos(int pos);
    void printfGnuGoStruct();
    bool isValidPos(int row, int col); //need extra checks, because is_valid() from GnuGo actually uses fucking asserts
    int populateStructFromGnuGo(); //populate our own structure from GnuGo; this will keep to a minimum places where the useGNUGO is used
    void updateSizes();


    void launchGame();
};

#endif // GOTABLE_H