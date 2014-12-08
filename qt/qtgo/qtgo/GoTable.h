#ifndef GOTABLE_H
#define GOTABLE_H


//#include <QWindow>
#include <QWidget>

//class QExposeEvent;

class GoTable : public QWidget {
Q_OBJECT
public:
    explicit GoTable(QWidget* parent = 0);
    ~GoTable();

    //virtual void render(QPainter* painter);
    void paintEvent(QPaintEvent *);

public slots:
//    void renderLater();
//    void renderNow();

protected:
    //bool event(QEvent *event);

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
    QPixmap* blackStonePixmap;
    QPixmap* whiteStonePixmap;
    float dist;
    int highlightRow;
    int highlightCol;
    int newStoneRow;
    int newStoneCol;
    //bool highlightPosChanged;

    int player;

private:
    bool buildPixmaps(int diameter);
    void updateCursor();
    int placeStone(int row, int col);
};

#endif // GOTABLE_H
