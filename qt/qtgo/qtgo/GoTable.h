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

private:
    bool m_updatePending;
    QBackingStore* m_backingStore;
    QCursor* blackCursor = NULL;
    QCursor* whiteCursor = NULL;

private:
    bool buildCursors(int diameter);
};

#endif // GOTABLE_H
