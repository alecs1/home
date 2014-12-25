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
    bool m_updatePending;
    QBackingStore* m_backingStore;
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

    SGameSettings settings;

    int player;

    bool useGNUGO = true;

private:
    bool buildPixmaps(int diameter);
    void updateCursor();
    bool placeStone(int row, int col);

    void initGnuGo();
    int toGnuGoPos(int row, int col);
    void printfGnuGoStruct();
    bool isValidPos(int row, int col); //because is_valid() from GnuGo actually uses fucking asserts
    int populateStructFromGnuGo(); //populate our own structure from GnuGo; this will keep to a minimum places where the useGNUGO is used
    void updateSizes();


    void launchGame();
};

#endif // GOTABLE_H
