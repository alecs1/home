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

    blackCursor = NULL;
    whiteCursor = NULL;
    blackStonePixmap = NULL;
    whiteStonePixmap = NULL;
    highlightCol = -1;
    highlightRow = -1;
    newStoneRow = -1;
    newStoneCol = -1;
}

GoTable::~GoTable() {
    printf("Implement destructor!\n");
}

void GoTable::exposeEvent(QExposeEvent* event) {
    if (isExposed()) {
        renderNow();
    }
}

void GoTable::mouseMoveEvent(QMouseEvent* ev) {
    QPoint pos = mouseToGameCoordinates(ev);
    int row = pos.y();
    int col = pos.x();

    if (highlightRow != row || highlightCol != col) {
        highlightRow = row;
        highlightCol = col;
        highlightPosChanged = true; //kind of unused; maybe I should not call renderNow() directly?
        renderNow();
    }

    printf("mouseMoveEvent - localPos=%f, %f, row=%d, col=%d\n", pos.ry(), pos.rx(), row, col);
}

void GoTable::mousePressEvent(QMouseEvent* ev) {
    QPoint pos = mouseToGameCoordinates(ev);
    highlightRow = pos.y();
    highlightCol = pos.x();

    newStoneRow = pos.y();
    newStoneCol = pos.x();

    QPointF localPos = ev->localPos();
    printf("mousePressEvent - %f, %f -> %d, %d\n", localPos.ry(), localPos.rx(), pos.y(), pos.x());
}

void GoTable::mouseReleaseEvent(QMouseEvent* ev) {
    QPoint pos = mouseToGameCoordinates(ev);
    if (pos.x() == newStoneCol && pos.y() == newStoneCol) {
        placeStone(pos.y(), pos.x());
    }
    newStoneRow = -1;
    newStoneCol = -1;

    QPointF localPos = ev->localPos();
    printf("mouseReleaseEvent - %f, %f -> %d, %d\n", localPos.ry(), localPos.rx(), pos.y(), pos.x());
}

QPoint GoTable::mouseToGameCoordinates(QMouseEvent* ev) {
    QPointF localPos = ev->localPos();

    //compute the closest intersection
    int row = localPos.ry() / dist - 0.5;
    int col = localPos.rx() / dist - 0.5;
    if ( row >= 0 && row < game.size && col >= 0 && col < game.size)
        return QPoint(col, row);
    else
        return QPoint(-1, -1);
}

void GoTable::resizeEvent(QResizeEvent* event) {
    printf("resizeEvent\n");

    int tableSize = width(); //compute and enforce correctly
    if (height() < tableSize)
        tableSize = height();
    dist = tableSize / (game.size + 1.0);
    float lineWidth = tableSize / 300.0;
    int diameter = (int) (dist * 0.8);

    buildPixmaps(diameter);

    m_backingStore->resize(event->size());
    if (isExposed())
        renderNow();

    setCursor(*blackCursor);
    highlightPosChanged = true;
}


void GoTable::renderNow() {
    printf("renderNow\n");
    if (!isExposed())
        return;

    QRect rect(0, 0, width(), height());
    m_backingStore->beginPaint(rect);

    QPaintDevice* device = m_backingStore->paintDevice();
    QPainter painter(device);

    painter.fillRect(0, 0, width(), height(), QColor(180, 210, 50));
    render(&painter);

    m_backingStore->endPaint();
    m_backingStore->flush(rect);

}

//paint logic goes here
void GoTable::render(QPainter *painter) {
    //painter->drawText(QRectF(0, 0, width(), height()), Qt::AlignHCenter, QStringLiteral("Implement me!"));

    //TODO - all of this has to go to off-screen buffers and be called only on resize
    int tableSize = width(); //compute and enforce correctly
    if (height() < tableSize)
        tableSize = height();
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

    if ((highlightRow != - 1) && (highlightPosChanged)) {
        highlightPosChanged = false;
        QPointF highlightPos(dist + highlightCol * dist, dist + highlightRow * dist);
        printf("render, highlight at: %f, %f\n", highlightPos.rx(), highlightPos.ry());
        float highlightDiameter = dist / 2;

        pen.setColor(QColor(256, 100, 100, 128));
        pen.setWidthF(dist/8);
        painter->setRenderHint(QPainter::Antialiasing);
        painter->setPen(pen);
        painter->drawEllipse(highlightPos, highlightDiameter, highlightDiameter);
    }

    if (newStoneCol != -1) {
        QPointF newStonePos(dist + newStoneCol * dist, dist + newStoneRow * dist);
        painter->drawPixmap(newStonePos, *blackStonePixmap);
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

bool GoTable::buildPixmaps(int diameter) {
    printf("buildCursors, diameter=%d\n", diameter);
    QSvgRenderer svgR;

    delete blackStonePixmap;
    blackStonePixmap = new QPixmap(diameter, diameter);
    blackStonePixmap->fill(Qt::transparent);
    svgR.load(QString("resources/cursorBlack.svg"));
    QPainter bPainter(blackStonePixmap);
    svgR.render(&bPainter);
    delete blackCursor;
    blackCursor = new QCursor(*blackStonePixmap);

    delete whiteStonePixmap;
    whiteStonePixmap = new QPixmap(diameter, diameter);
    whiteStonePixmap->fill(QColor(0, 0, 0, 0));
    svgR.load(QString("resources/cursorWhite.svg"));
    QPainter wPainter(whiteStonePixmap);
    svgR.render(&wPainter);
    delete whiteCursor;
    whiteCursor = new QCursor(*whiteStonePixmap);

    return true;
}


//some game logic
int GoTable::placeStone(int row, int col) {
    printf("placeStone: %d, %d\n", row, col);
    int retVal = -1;
    //retVal = placeStone(game, row, col);
    return retVal;
}

