#include <QBackingStore>
#include <QResizeEvent>
#include <QPainter>
#include <QCoreApplication>
#include <QMainWindow>
#include <QSvgRenderer>

#include "GoTable.h"
#include "GameStruct.h"

extern GameStruct game;

/**/
GoTable::GoTable(QWindow *parent) :
    QWindow(parent),
    m_updatePending(false)
{
    m_backingStore = new QBackingStore(this);
    create();

    setGeometry(100, 100, 100, 100);
}

GoTable::~GoTable() {
    printf("Implement destructor!\n");
}

void GoTable::exposeEvent(QExposeEvent* event) {
    if (isExposed()) {
        renderNow();
    }
}

void GoTable::resizeEvent(QResizeEvent* event) {
    printf("resizeEvent\n");

    int tableSize = width(); //compute and enforce correctly
    if (height() < tableSize)
        tableSize = height();
    float dist = tableSize / (game.size + 1.0);
    float lineWidth = tableSize / 300.0;
    int diameter = (int) (dist * 0.8);

    buildCursors(diameter);

    m_backingStore->resize(event->size());
    if (isExposed())
        renderNow();

    setCursor(*blackCursor);
}


void GoTable::renderNow() {
    printf("renderNow\n");
    if (!isExposed())
        return;

    QRect rect(0, 0, width(), height());
    m_backingStore->beginPaint(rect);

    QPaintDevice* device = m_backingStore->paintDevice();
    QPainter painter(device);

    painter.fillRect(0, 0, width(), height(), Qt::yellow);
    render(&painter);

    m_backingStore->endPaint();
    m_backingStore->flush(rect);

}

//paint logic goes here
void GoTable::render(QPainter *painter) {
    //painter->drawText(QRectF(0, 0, width(), height()), Qt::AlignHCenter, QStringLiteral("Implement me!"));

    int tableSize = width(); //compute and enforce correctly
    if (height() < tableSize)
        tableSize = height();
    float dist = tableSize / (game.size + 1.0);
    float lineWidth = tableSize / 300.0;

    QPen pen;
    pen.setWidthF(lineWidth);
    painter->setPen(pen);
    printf("render; width=%d, height=%d, tableSize=%d, dist=%f, lineWidth=%f\n", width(), height(), tableSize, dist, lineWidth);
    //horizontal
    for(int i = 0; i < game.size; i++) {
        painter->drawLine(QLineF(dist, dist + i * dist, tableSize-dist, dist + i * dist));
    }
    for (int i = 0; i < game.size; i++) {
        painter->drawLine(QLineF(dist + i * dist, dist, dist + i * dist, tableSize - dist));
    }
}



void GoTable::renderLater() {
    if (!m_updatePending) {
        m_updatePending = true;
        QCoreApplication::postEvent(this, new QEvent(QEvent::UpdateRequest));
    }
}

bool GoTable::event(QEvent *event) {
    if (event->type() == QEvent::UpdateRequest) {
        m_updatePending = false;
        renderNow();
        return true;
    }
    return QWindow::event(event);
}

bool GoTable::buildCursors(int diameter) {
    printf("buildCursors, diameter=%d\n", diameter);
    QSvgRenderer svgR;

    QPixmap blackPixmap(diameter, diameter);
    blackPixmap.fill(Qt::transparent);
    svgR.load(QString("resources/cursorBlack.svg"));
    QPainter bPainter(&blackPixmap);
    svgR.render(&bPainter);
    delete blackCursor;
    blackCursor = new QCursor(blackPixmap);

    QPixmap whitePixmap(diameter, diameter);
    whitePixmap.fill(QColor(0, 0, 0, 0));

    svgR.load(QString("resources/cursorWhite.svg"));
    QPainter wPainter(&whitePixmap);
    svgR.render(&wPainter);
    delete whiteCursor;
    whiteCursor = new QCursor(whitePixmap);

    return true;
}


