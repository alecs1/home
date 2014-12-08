#ifndef GOTABLE_H
#define GOTABLE_H


#include <QWindow>

class QExposeEvent;

class GoTable : public QWindow {
Q_OBJECT
public:
    explicit GoTable(QWindow* parent = 0);
    ~GoTable();

    virtual void render(QPainter* painter);

public slots:
    void renderLater();
    void renderNow();

protected:
    bool event(QEvent *event);

    void resizeEvent(QResizeEvent *event);
    void exposeEvent(QExposeEvent* event);

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
    bool highlightPosChanged;

private:
    bool buildPixmaps(int diameter);
    int placeStone(int row, int col);
};

#endif // GOTABLE_H
