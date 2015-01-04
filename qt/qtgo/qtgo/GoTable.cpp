#include <QBackingStore>
#include <QResizeEvent>
#include <QPainter>
#include <QCoreApplication>
#include <QMainWindow>
#include <QSvgRenderer>
#include <QTimer>

extern "C" {
#include "engine/board.h"
int do_genmove(int color, float pure_threat_value,
              int allowed_moves[BOARDMAX], float *value, int *resign);
void
init_gnugo(float memory, unsigned int seed);
}

#include "GoTable.h"

#include "GameStruct.h"

QList<QString> rowNumbering { "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19" };
QList<QString> colNumbering { "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T" };

extern GameStruct game;


GoTable::GoTable(QWidget *parent) :
    QWidget(parent)
{
    setMouseTracking(true);

    setMinimumWidth(400);
    setMinimumHeight(400);

    blackCursor = NULL;
    whiteCursor = NULL;
    redCursor = NULL;
    blackStonePixmap = NULL;
    whiteStonePixmap = NULL;
    redStonePixmap = NULL;
    highlightCol = -1;
    highlightRow = -1;
    newStoneRow = -1;
    newStoneCol = -1;

    game.size = settings.size;
    players[BLACK]= settings.black;
    players[WHITE] = settings.white;

    buildPixmaps(10);
    setCursor(*blackCursor);

    if (useGNUGO) {
        init_gnugo(50, 314);
        resetGnuGo();
    }

    crtPlayer = BLACK;
    state = GameState::Initial;
    emit GameStateChanged(state);
}

GoTable::~GoTable() {
    printf("Implement destructor!\n");
}

void GoTable::launchGamePressed(SGameSettings newSettings) {
    settings = newSettings;

    printf("%s\n", __func__);

    if (state == GameState::Started) {
        state = GameState::Stopped;
        //TODO - ask if we want to lose progress
    }
    else {
        launchGame();
        state = GameState::Started;
    }

    updateCursor();
    emit GameStateChanged(state);
}

//void GoTable::settingsChanged(SGameSettings newSettings) {
//    //some setting will take effect right away, others won't, check them here
//    settings = newSettings;
//}

bool GoTable::AIPlayNextMove() {

    //genmove(int color, float *value, int *resign)
    //static int do_genmove(int color, float pure_threat_value,
    //              int allowed_moves[BOARDMAX], float *value, int *resign);

    float value;
    int resign;
    int move = 0;
    move = do_genmove(crtPlayer, 0.5, NULL, &value, &resign);

    printfGnuGoStruct();
    printf("%s, move:%d, value=%f, resign=%d\n", __func__, move, value, resign);
    QPoint point = fromGnuGoPos(move);
    placeStone(point.y(), point.x());

    printfGnuGoStruct();

    return false;
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

    //QPointF localPos = ev->localPos();
    //printf("%s - localPos=%f, %f, row=%d, col=%d\n", __func__, localPos.ry(), localPos.rx(), row, col);
}

void GoTable::mousePressEvent(QMouseEvent* ev) {
    if (players[crtPlayer] != PlayerType::LocalHuman)
        return;

    QPoint pos = mouseToGameCoordinates(ev);
    highlightRow = pos.y();
    highlightCol = pos.x();

    if (GameCanPlaceStone(&game, pos.y(), pos.x(), crtPlayer)) {
        newStoneRow = pos.y();
        newStoneCol = pos.x();
    }

    //TODO - this one can also be optimised
    update();

    QPointF localPos = ev->localPos();
    printf("%s - %f, %f -> %d, %d\n", __func__, localPos.ry(), localPos.rx(), pos.y(), pos.x());
}

void GoTable::mouseReleaseEvent(QMouseEvent* ev) {
    if (players[crtPlayer] != PlayerType::LocalHuman)
        return;

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
    Q_UNUSED(event);
    printf("resizeEvent\n");
    updateSizes();
}

void GoTable::updateSizes() {
    int tableSize = width(); //compute and enforce correctly
    if (height() < tableSize)
        tableSize = height();
    dist = tableSize / (game.size + 1.0);
    int diameter = (int) (dist * 0.8);

    printf("%s - game.size=%d, tableSize=%d, dist=%f, diameter=%d\n", __func__, game.size, tableSize, dist, diameter);

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


    //lines
    QPen pen;
    pen.setWidthF(lineWidth);

    painter.setPen(pen);
    //printf("%s - width=%d, height=%d, tableSize=%d, dist=%f, lineWidth=%f\n", __func__, width(), height(), tableSize, dist, lineWidth);

    //Horizontal lines y is resolved to an int, lines will have exactly same width without AA
    for(int i = 0; i < game.size; i++) {
        int y = dist + i * dist;
        painter.drawLine(QLineF(dist, y, tableSize-dist, y));
    }
    //Vertical x resolved to an int...
    for (int i = 0; i < game.size; i++) {
        int x = dist + i * dist;
        painter.drawLine(QLineF(x, dist, x, tableSize - dist));
    }


    //numbering: from bottom left corner
    //consider the simbol should fit into a square with length 1/3 of dist;
    //for perfect results will use QFontMetrics, for now we make assumptions
    QFont font("DejaVu Sans", dist/3.5);
    painter.setFont(font);
    qreal margin = 1.0/8 * dist;
    for (int i = 0 ; i < game.size; i++) {
        painter.drawText(QPointF(margin, dist + i * dist), rowNumbering[game.size-i-1]);
        painter.drawText(QPointF(tableSize - 1.0/2.5 * dist - margin, dist + i * dist), rowNumbering[game.size-i-1]);
    }

    for (int i = 0; i < game.size; i++) {
        painter.drawText(QPointF(dist + i * dist, 1.0/3 * dist + margin), colNumbering[i]);
        painter.drawText(QPointF(dist + i * dist, tableSize - margin), colNumbering[i]);
    }

    //highlighted position
    if (highlightRow != - 1) {
        QPointF highlightPos(dist + highlightCol * dist, dist + highlightRow * dist);
        //printf("%s - highlight at: %f, %f\n", __func__, highlightPos.rx(), highlightPos.ry());
        float highlightDiameter = dist / 2;

        pen.setColor(QColor(255, 50, 50, 128));
        pen.setWidthF(dist/8);
        painter.setRenderHint(QPainter::Antialiasing);
        painter.setPen(pen);
        painter.drawEllipse(highlightPos, highlightDiameter, highlightDiameter);
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

    delete redStonePixmap;
    redStonePixmap = new QPixmap(diameter, diameter);
    redStonePixmap->fill(QColor(0, 0, 0, 0));
    svgR.load(QString("resources/cursorRed.svg"));
    QPainter rPainter(redStonePixmap);
    svgR.render(&rPainter);
    delete redCursor;
    redCursor = new QCursor(*redStonePixmap);

    return true;
}


//some game logic
bool GoTable::placeStone(int row, int col) {
    printf("placeStone: %d, %d, %d\n", row, col, crtPlayer);
    if (!isValidPos(row, col))
        return false;

    if (state == GameState::Stopped)
        return false;

    bool retVal = false;
    if (useGNUGO) {
        int pos = toGnuGoPos(row, col);
        bool canPlay = is_legal(pos, crtPlayer);

        if (canPlay) {
            play_move(pos, crtPlayer);
            if (crtPlayer == WHITE)
                crtPlayer = BLACK;
            else
                crtPlayer = WHITE;
            updateCursor();
            retVal = true;
        }
        printfGnuGoStruct();
        populateStructFromGnuGo();
    }
    else {
        retVal = GamePlaceStone(&game, row, col, crtPlayer);
        if (retVal == true) {
            if (crtPlayer == WHITE)
                crtPlayer = BLACK;
            else
                crtPlayer = WHITE;
            updateCursor();
        }
    }

    if (retVal && state == GameState::Initial) {
        printf("%s - we just automatically started a new game!\n", __func__);
        state = GameState::Started;
        emit GameStateChanged(state);
    }

    update();

    //depending on the type of the next player, we might need to play one more move, without recursing into this function :D
    if (players[crtPlayer] == PlayerType::AI)
        QTimer::singleShot(0, this, SLOT(AIPlayNextMove()));

    return retVal;
}

//change colour of mouse cursor to reflect the current player
void GoTable::updateCursor() {
    if (state == GameState::Stopped)
        setCursor(*redCursor);
    else if (crtPlayer == BLACK)
        setCursor(*blackCursor);
    else
        setCursor(*whiteCursor);
}

void GoTable::resetGnuGo() {
    board_size = settings.size;
    clear_board();
    printfGnuGoStruct();
}

int GoTable::toGnuGoPos(int row, int col) {
    return (row+1) * 20 + col + 1;
}

QPoint GoTable::fromGnuGoPos(int pos) {
    int row = pos / 20 - 1;
    int col = pos - (row+1)*20 - 1;
    return QPoint(col, row);
}

void GoTable::printfGnuGoStruct() {
    for (int i = 0; i < 21; i++) {
        for (int j = 0; j < 20; j++)
            printf("%d", board[i*20+j]);
        printf("\n");
    }
}

bool GoTable::isValidPos(int row, int col) {
    if (row < 0 || row >= game.size || col < 0 || col >= game.size) {
        return false;
    }
    return true;
}

int GoTable::populateStructFromGnuGo() {
    for (int i = 0; i < game.size; i++) {
        for(int j = 0; j < game.size; j++)
            game.state[i][j] = board[toGnuGoPos(i, j)];
    }
    return 0;
}

void GoTable::launchGame() {
    game.size = settings.size;
    players[BLACK] = settings.black;
    players[WHITE] = settings.white;
    updateSizes();
    if (useGNUGO) {
        resetGnuGo();
        populateStructFromGnuGo();
    }
    update();
    if (players[crtPlayer] == PlayerType::AI) {
        QTimer::singleShot(0, this, SLOT(AIPlayNextMove()));
    }
}
