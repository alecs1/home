#include "GoTableWidget.h"

#include <QPainter>
#include <QSvgRenderer>

extern "C" {
extern int best_moves[10];
extern float best_move_values[10];
}

#include "Utils.h"

const QString fontName = "Liberation Sans";

void GoTableWidget::paint(QPaintDevice* target) const {
    const QColor colourMoveAllowed(0, 255, 0);
    const QColor colourMoveDenied(255, 0, 0);
    
    int tableSize = width(); //compute and enforce correctly
    if (height() < tableSize)
        tableSize = height();
    float lineWidth = tableSize / 300.0;

    //TODO - move these to a common place for both table and DrawArea
    QColor mainColor(0, 0, 0);
    QColor highlightPosColour = colourMoveAllowed;
    if (players[crtPlayer] != PlayerType::LocalHuman) {
        highlightPosColour = colourMoveDenied;
    }
    highlightPosColour.setAlpha(150);
    QColor highlightLineColour = highlightPosColour;
    highlightLineColour.setAlpha(255);
    
    QPainter painter(target);

    //lines
    QPen pen;
    pen.setWidthF(lineWidth);
    pen.setColor(mainColor);
    painter.setPen(pen);

    QPen lineHighlightPen = pen;
    lineHighlightPen.setColor(highlightLineColour);
    //the highlighted line will never be tinner than 2 pixels
    float aux = lineWidth * 1.5;
    if (aux < 2)
        aux = 2;
    lineHighlightPen.setWidthF(aux);

    //Horizontal lines y is resolved to an int, lines will have exactly same width without AA
    for(int i = 0; i < game.size; i++) {
        if (i == highlightRow)
            continue;
        int y = dist + i * dist;
        painter.drawLine(QLineF(dist, y, game.size*dist, y));
    }
    //Vertical x resolved to an int...
    for (int i = 0; i < game.size; i++) {
        if (i == highlightCol)
            continue;
        int x = dist + i * dist;
        painter.drawLine(QLineF(x, dist, x, game.size*dist));
    }


    //highlighted position
    if (highlightRow != - 1) {
        painter.setPen(lineHighlightPen);
        painter.drawLine(QLineF(dist, dist + highlightRow*dist,
                                 game.size*dist, dist + highlightRow*dist));
        painter.drawLine(QLineF(dist + highlightCol*dist, dist,
                                dist + highlightCol*dist, game.size*dist));

        QPointF highlightPos(dist + highlightCol * dist, dist + highlightRow * dist);
        float RADIUS_SCALE = 1.3;
        float highlightRadius = dist / RADIUS_SCALE;

        pen.setColor(highlightPosColour);
        pen.setWidthF(dist/5);
        painter.setRenderHint(QPainter::Antialiasing);
        painter.setPen(pen);
        painter.drawEllipse(highlightPos, highlightRadius, highlightRadius);
    }

    //higlight last move
    if (lastMoveRow > -1) {
        QPointF lastMovePos(dist + lastMoveCol * dist, dist + lastMoveRow * dist);
        float RADIUS_SCALE = 2;
        float highlightRadius = dist / RADIUS_SCALE;
        pen.setColor(QColor(0, 170, 0, 150));
        pen.setWidthF(dist / 2);
        painter.setRenderHint(QPainter::Antialiasing);
        painter.setPen(pen);
        painter.drawEllipse(lastMovePos, highlightRadius, highlightRadius);
    }

    //hints, the last move must be painted over the hints
    if (showHints) {
        for(int i = 0; i < 10; i++) {
            QPoint move = fromGnuGoPos(best_moves[i]);
            if (move.x() < 0)
                continue;

            float val = best_move_values[i];
            QPointF moveHintPos(dist + move.x()*dist, dist + move.y()*dist);
            const float drawDiameter = 0.9 * dist;
            QRectF textRect(moveHintPos.x() - drawDiameter/2, moveHintPos.y() - drawDiameter/2,
                            drawDiameter, drawDiameter);

            //more transparent as the move is considered weaker
            QColor backColour (32, 32, 64, 220 - 20 * i);
            pen.setWidthF(0);
            pen.setColor(backColour);
            painter.setPen(pen);
            painter.setBrush(QBrush(backColour, Qt::SolidPattern));
            painter.drawEllipse(textRect);

            pen.setColor(QColor(255, 255, 255, 255));
            painter.setPen(pen);
            QString printable;
            int pointSize = -1;
            //TODO - these sizes must be computed at resize time only, avoid expensive stuff.
            int auxSmaller, auxLarger;
            Utils::PointSizeParams p;
            p.fontName = fontName;
            p.targetSize = drawDiameter;
            p.nextSmaller = &auxSmaller;
            p.nextLarger = &auxLarger;
            p.measure = Utils::PointSizeParams::Measure::heightAscent;
            p.text = "0";
            if (platformType() == PlatformType::Android) {
                //smaller screen, we'll show shorter hints
                printable.sprintf("%d", 9 - i);
                pointSize = Utils::getClosestPointSize(p);
            }
            else {
                //I think this is useless even on desktop
                printable.sprintf("%1.1f", val);
                p.measure = Utils::PointSizeParams::Measure::width;
                p.text = "22.2";
                pointSize = Utils::getClosestPointSize(p);
            }

            QFont font = QFont(fontName, pointSize);
            painter.setFont(font);
            painter.drawText(textRect, Qt::AlignCenter, printable);
        }
    }

    //new stone, mouse button pressed/tapped
    if ((newStoneCol != -1) && (state != GameState::Stopped)){
        QPointF newStonePos(dist + newStoneCol * dist - blackStonePixmap->width()/2, dist + newStoneRow * dist - blackStonePixmap->width()/2);
        //printf("%s - highlight at: %f, %f\n", __func__, newStonePos.rx(), newStonePos.ry());
        if (crtPlayer == BLACK)
            painter.drawPixmap(newStonePos, *blackStonePixmap);
        else if (crtPlayer == WHITE)
            painter.drawPixmap(newStonePos, *whiteStonePixmap);
    }

    if ((unconfirmedStoneCol != -1) && (state != GameState::Stopped)){
        QPointF unconfirmedStonePos(dist + unconfirmedStoneCol * dist - blackStonePixmap->width()/2, dist + unconfirmedStoneRow * dist - blackStonePixmap->width()/2);
        //printf("%s - highlight at: %f, %f\n", __func__, newStonePos.rx(), newStonePos.ry());
        if (crtPlayer == BLACK)
            painter.drawPixmap(unconfirmedStonePos, *blackStonePixmap);
        else if (crtPlayer == WHITE)
            painter.drawPixmap(unconfirmedStonePos, *whiteStonePixmap);
    }

    //all stones already on the table
    for(int i = 0; i < game.size; i++) {
        for(int j = 0; j < game.size; j++) {
            if (game.state[i][j] > 0) {
                QPointF stonePos(dist + j*dist - blackStonePixmap->width()/2, dist + i*dist - blackStonePixmap->width()/2);
                int colour = 0;
                colour = game.state[i][j];

                if (colour == BLACK)
                    painter.drawPixmap(stonePos, *blackStonePixmap);
                else if (colour == WHITE)
                    painter.drawPixmap(stonePos, *whiteStonePixmap);
            }
        }
    }
}


void GoTableWidget::paintEvent(QPaintEvent *) {
    setAttribute(Qt::WA_TranslucentBackground); //only needed on Android
    paint(this);
}

bool GoTableWidget::buildPixmaps(int diameter) {
    QSvgRenderer svgR;

    delete blackStonePixmap;
    blackStonePixmap = new QPixmap(diameter, diameter);
    blackStonePixmap->fill(Qt::transparent);
    svgR.load(QString(":/resources/cursorBlack.svg"));
    QPainter bPainter(blackStonePixmap);
    svgR.render(&bPainter);
    delete blackCursor;
    blackCursor = new QCursor(*blackStonePixmap);

    delete whiteStonePixmap;
    whiteStonePixmap = new QPixmap(diameter, diameter);
    whiteStonePixmap->fill(QColor(0, 0, 0, 0));
    svgR.load(QString(":/resources/cursorWhite.svg"));
    QPainter wPainter(whiteStonePixmap);
    svgR.render(&wPainter);
    delete whiteCursor;
    whiteCursor = new QCursor(*whiteStonePixmap);

    delete redStonePixmap;
    redStonePixmap = new QPixmap(diameter, diameter);
    redStonePixmap->fill(QColor(0, 0, 0, 0));
    svgR.load(QString(":/resources/cursorRed.svg"));
    QPainter rPainter(redStonePixmap);
    svgR.render(&rPainter);
    delete redCursor;
    redCursor = new QCursor(*redStonePixmap);

    return true;
}
