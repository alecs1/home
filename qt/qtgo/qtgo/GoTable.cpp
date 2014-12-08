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
GoTable::GoTable(QWidget *parent) :
    QWidget(parent),
    m_updatePending(false)
{
    m_backingStore = backingStore();
    create();
    setMouseTracking(true);

    setGeometry(100, 100, 100, 100);

    blackCursor = NULL;
    whiteCursor = NULL;
    blackStonePixmap = NULL;
    whiteStonePixmap = NULL;
    highlightCol = -1;
    highlightRow = -1;
    newStoneRow = -1;
    newStoneCol = -1;

    buildPixmaps(10);
    setCursor(*blackCursor);

    player = 1;
}

GoTable::~GoTable() {
    printf("Implement destructor!\n");
}


void GoTable::mouseMoveEvent(QMouseEvent* ev) {
    QPoint pos = mouseToGameCoordinates(ev);
    int row = pos.y();
    int col = pos.x();

    if (highlightRow != row || highlightCol != col) {
        highlightRow = row;
        highlightCol = col;
        update();
    }

    QPointF localPos = ev->localPos();
    printf("%s - localPos=%f, %f, row=%d, col=%d\n", __func__, localPos.ry(), localPos.rx(), row, col);
}

void GoTable::mousePressEvent(QMouseEvent* ev) {
    QPoint pos = mouseToGameCoordinates(ev);
    highlightRow = pos.y();
    highlightCol = pos.x();

    if (GameCanPlaceStone(&game, pos.y(), pos.x(), player)) {
        newStoneRow = pos.y();
        newStoneCol = pos.x();
    }

    //TODO - this one can also be optimised
    update();

    QPointF localPos = ev->localPos();
    printf("%s - %f, %f -> %d, %d\n", __func__, localPos.ry(), localPos.rx(), pos.y(), pos.x());
}

void GoTable::mouseReleaseEvent(QMouseEvent* ev) {
    QPoint pos = mouseToGameCoordinates(ev);

    QPointF localPos = ev->localPos();
    printf("%s - %f, %f -> %d, %d\n", __func__, localPos.ry(), localPos.rx(), pos.y(), pos.x());
    if (pos.x() == newStoneCol && pos.y() == newStoneRow) {
        placeStone(pos.y(), pos.x());
    }
    newStoneRow = -1;
    newStoneCol = -1;

    //TODO - this can be optimised
    update();
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
    int diameter = (int) (dist * 0.8);

    buildPixmaps(diameter);
    updateCursor();
}


void GoTable::paintEvent(QPaintEvent *) {

    //TODO - all of this has to go to off-screen buffers and be called only on resize
    int tableSize = width(); //compute and enforce correctly
    if (height() < tableSize)
        tableSize = height();
    float lineWidth = tableSize / 300.0;

    QPainter painter(this);

    //background
    QColor background(206, 170, 57);
    painter.fillRect(QRectF(0, 0, width(), height()), background);

    QPen pen;
    pen.setWidthF(lineWidth);

    painter.setPen(pen);
    printf("%s - width=%d, height=%d, tableSize=%d, dist=%f, lineWidth=%f\n", __func__, width(), height(), tableSize, dist, lineWidth);

    //lines
    for(int i = 0; i < game.size; i++) {
        painter.drawLine(QLineF(dist, dist + i * dist, tableSize-dist, dist + i * dist));
    }
    for (int i = 0; i < game.size; i++) {
        painter.drawLine(QLineF(dist + i * dist, dist, dist + i * dist, tableSize - dist));
    }

    //highlighted position
    if (highlightRow != - 1) {
        QPointF highlightPos(dist + highlightCol * dist, dist + highlightRow * dist);
        printf("%s - highlight at: %f, %f\n", __func__, highlightPos.rx(), highlightPos.ry());
        float highlightDiameter = dist / 2;

        pen.setColor(QColor(255, 50, 50, 128));
        pen.setWidthF(dist/8);
        painter.setRenderHint(QPainter::Antialiasing);
        painter.setPen(pen);
        painter.drawEllipse(highlightPos, highlightDiameter, highlightDiameter);
    }

    //new stone, mouse button pressed/tapped
    if (newStoneCol != -1) {
        QPointF newStonePos(dist + newStoneCol * dist - blackStonePixmap->width()/2, dist + newStoneRow * dist - blackStonePixmap->width()/2);
        printf("%s - highlight at: %f, %f\n", __func__, newStonePos.rx(), newStonePos.ry());
        painter.drawPixmap(newStonePos, *blackStonePixmap);
    }

    //all stones already on the table
    for(int i = 0; i < game.size; i++) {
        for(int j = 0; j < game.size; j++) {
            if (game.state[i][j] > 0) {
                QPointF stonePos(dist + j*dist - blackStonePixmap->width()/2, dist + i*dist - blackStonePixmap->width()/2);
                if (game.state[i][j] == 1)
                    painter.drawPixmap(stonePos, *blackStonePixmap);
                else
                    painter.drawPixmap(stonePos, *whiteStonePixmap);
            }
        }
    }

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
    printf("placeStone: %d, %d, %d\n", row, col, player);
    int retVal = -1;
    retVal = GamePlaceStone(&game, row, col, player);
    if (retVal == true) {
        if (player == 1)
            player = 2;
        else
            player = 1;
        updateCursor();
    }
    return retVal;
}

void GoTable::updateCursor() {
    if (player == 1)
        setCursor(*blackCursor);
    else
        setCursor(*whiteCursor);
}


