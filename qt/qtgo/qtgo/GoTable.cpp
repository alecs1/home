#include <QBackingStore>
#include <QResizeEvent>
#include <QPainter>
#include <QCoreApplication>
#include <QMainWindow>
#include <QSvgRenderer>
#include <QTimer>
#include <QTime>
#include <QTextStream>
#include <QMutex>

extern "C" {
#include "engine/board.h"
int do_genmove(int color, float pure_threat_value, int allowed_moves[BOARDMAX], float *value, int *resign);
void init_gnugo(float memory, unsigned int seed);
//void compute_scores(int use_chinese_rules);
float gnugo_estimate_score(float *upper, float *lower);
}

#include "GoTable.h"
#include "GameStruct.h"
#include "GameEndDialog.h"

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
    unconfirmedStoneRow = -1;
    unconfirmedStoneCol = -1;

    askPlayConfirmation = false;
    #if defined(Q_OS_ANDROID)
    askPlayConfirmation = true;
    #endif

    game.size = settings.size;
    players[EMPTY] = PlayerType::None;
    players[BLACK]= settings.black;
    players[WHITE] = settings.white;

    gnuGoMutex = new QMutex;
    aiThread = new AIThread(gnuGoMutex);


    blockTime = new QTime();


    buildPixmaps(10);
    setCursor(*blackCursor);

    if (useGNUGO) {
        init_gnugo(50, 314);
        resetGnuGo();
    }

    crtPlayer = BLACK;
    emit crtPlayerChanged(crtPlayer, players[crtPlayer]);

    state = GameState::Initial;
    emit gameStateChanged(state);

    connect(aiThread, SIGNAL(AIThreadPlaceStone(int,int)), this, SLOT(placeStone(int,int)));
    connect(aiThread, SIGNAL(AIQuitsGame()), this, SLOT(finish()));
}

GoTable::~GoTable() {
    printf("%s - Implement destructor!\n", __func__);
}

void GoTable::launchGamePressed(SGameSettings newSettings) {
    settings = newSettings;

    printf("%s\n", __func__);

    if (state == GameState::Started) {
        state = GameState::Stopped;
        finish();
        //TODO - ask if we want to lose progress
    }
    else {
        launchGame();
        state = GameState::Started;
    }

    updateCursor();
    emit gameStateChanged(state);
}

void GoTable::changeGameSettings(SGameSettings newSettings) {
    printf("%s\n", __func__);
    settings = newSettings;
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
    if (players[crtPlayer] != PlayerType::LocalHuman) {
        printf("%s - return because crtPlayer is not localHuman\n", __func__);
        return;
    }

    if (shouldRejectInput(ev)) {
        return;
    }

    QPoint pos = mouseToGameCoordinates(ev);
    highlightRow = pos.y();
    highlightCol = pos.x();

    if (GameCanPlaceStone(&game, pos.y(), pos.x(), crtPlayer)) {
        newStoneRow = pos.y();
        newStoneCol = pos.x();
        if (newStoneRow != unconfirmedStoneRow || newStoneCol != unconfirmedStoneCol) {
            unconfirmedStoneRow = -1;
            unconfirmedStoneCol = -1;
        }
    }

    //TODO - this one can also be optimised
    update();

    QPointF localPos = ev->localPos();
    //printf("%s - %f, %f -> %d, %d\n", __func__, localPos.ry(), localPos.rx(), pos.y(), pos.x());
}

void GoTable::mouseReleaseEvent(QMouseEvent* ev) {
    //static unsigned long lastTimestamp = ev->timestamp();
    //printf("timestamp: %lu, diff:%lu\n", ev->timestamp(), ev->timestamp() - lastTimestamp);
    //lastTimestamp = ev->timestamp();

    //printf("timestamp: %lu, lastInputTimestamp: %lu, diff:%lu, block duration:%lu\n",
    //       ev->timestamp(), lastInputTimestamp, ev->timestamp() - lastInputTimestamp, inputBlockingDuration);

    if (shouldRejectInput(ev)) {
        return;
    }
    else {
        lastInputTimestamp = ev->timestamp();
    }

    if (players[crtPlayer] != PlayerType::LocalHuman) {
        printf("%s - return because crtPlayer is not localHuman\n", __func__);
        return;
    }

    QPoint pos = mouseToGameCoordinates(ev);

    //QPointF localPos = ev->localPos();
    //printf("%s - %f, %f -> %d, %d\n", __func__, localPos.ry(), localPos.rx(), pos.y(), pos.x());
    if (pos.x() == newStoneCol && pos.y() == newStoneRow && newStoneCol != -1) {
        if (askPlayConfirmation) {
            if (unconfirmedStoneRow == newStoneRow && unconfirmedStoneCol == newStoneCol && acceptDoubleClickConfirmation) {
                //user confirmed by double clicking this
                placeStone(pos.y(), pos.x());
                newStoneRow = -1;
                newStoneCol = -1;
                unconfirmedStoneRow = -1;
                unconfirmedStoneCol = -1;
                emit askUserConfirmation(false);
            }
            else {
                unconfirmedStoneRow = newStoneRow;
                unconfirmedStoneCol = newStoneCol;
                newStoneRow = -1;
                newStoneCol = -1;
                emit askUserConfirmation(true);
            }
        }
        else {
            placeStone(pos.y(), pos.x());
        }
    }
    else if (askPlayConfirmation) {
        unconfirmedStoneRow = -1;
        unconfirmedStoneCol = -1;
        emit askUserConfirmation(false);
    }


    newStoneRow = -1;
    newStoneCol = -1;

    //TODO - this can be optimised
    update();
}

bool GoTable::shouldRejectInput(QMouseEvent* ev) {
    return false; //since we started threading this should rather make things worse

    if ((ev->timestamp() < lastInputTimestamp) || (inputBlockingDuration == 0)){
        //maybe the wm counter has been reset (every ~48 days :D)
        return false;
    }
    else {
        if ( (ev->timestamp() - lastInputTimestamp < inputBlockingDuration) &&
             (players[crtPlayer] == PlayerType::LocalHuman) ){
            printf("%s - rejected input as it's detected to be an old event from WM queue, "
                   "timestamp: %lu, lastInputTimestamp: %lu, dif: %lu, %lu\n",
                   __func__, ev->timestamp(), lastInputTimestamp, ev->timestamp() - lastInputTimestamp, inputBlockingDuration);
            return true;
        }
        return false;
    }
    return false;
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
    updateSizes();
}

void GoTable::updateSizes() {
    int tableSize = width(); //compute and enforce correctly
    if (height() < tableSize)
        tableSize = height();
    dist = tableSize / (game.size + 1.0);
    int diameter = (int) (dist * 0.95);

    //printf("%s - game.size=%d, tableSize=%d, dist=%f, diameter=%d\n", __func__, game.size, tableSize, dist, diameter);

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
    if (players[crtPlayer] == PlayerType::AI) {
        background = QColor(210, 200, 200);
    }
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
        float RADIUS_SCALE = 1.3;
        float highlightRadius = dist / RADIUS_SCALE;

        pen.setColor(QColor(255, 30, 30, 150));
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


bool GoTable::buildPixmaps(int diameter) {
    //printf("buildCursors, diameter=%d\n", diameter);
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


bool GoTable::placeStone(int row, int col) {
    printf("placeStone: %d, %d, %d\n", row, col, crtPlayer);
    inputBlockingDuration = 0;
    if (players[crtPlayer] == PlayerType::LocalHuman)
        blockTime->start();

    if (!isPosInsideTable(row, col))
        return false;

    if (state == GameState::Stopped)
        return false;

    cursorBlocked = true;
    updateCursor();

    bool retVal = false;
    if (useGNUGO) {
        int pos = toGnuGoPos(row, col);
        bool canPlay = is_legal(pos, crtPlayer);

        if (canPlay) {

            //TODO - maybe add wrapper for this call do centralise placing stones
            play_move(pos, crtPlayer);
            if (crtPlayer == WHITE)
                crtPlayer = BLACK;
            else
                crtPlayer = WHITE;
            retVal = true;
        }
        //printfGnuGoStruct();
        populateStructFromGnuGo();
    }
    else {
        retVal = GamePlaceStone(&game, row, col, crtPlayer);
        if (retVal == true) {
            if (crtPlayer == WHITE)
                crtPlayer = BLACK;
            else
                crtPlayer = WHITE;
        }
    }

    if (retVal) {
        emit crtPlayerChanged(crtPlayer, players[crtPlayer]);
        updateCursor();
        lastMoveRow = row;
        lastMoveCol = col;
    }

    if (retVal && state == GameState::Initial) {
        printf("%s - we just automatically started a new game!\n", __func__);
        state = GameState::Started;
        launchGame(false);
        emit gameStateChanged(state);
    }

    update();

    if (estimateScore) {
        printf("%s, calling gnugo_estimate_score, ts=%s\n", __func__, timer.getTimestampStr().toUtf8().constData());
        float score = gnugo_estimate_score(NULL, NULL);
        printf("%s, called gnugo_estimate_score, delta=%s\n", __func__, timer.getElapsedStr().toUtf8().constData());
        emit estimateScoreChanged(score);
        if (score > 0) {
            //printf("%s - estimates: white winning by %f\n", __func__, score);
        }
        else {
            //printf("%s - estimates: black winning by %f\n", __func__, -score);
        }
    }


    //depending on the type of the next player, we might need to play one more move, without recursing into this function :D
    if (players[crtPlayer] == PlayerType::AI) {
        QTimer::singleShot(2, this, SLOT(AIPlayNextMove()));
    }
    else {
        inputBlockingDuration = blockTime->elapsed();
        cursorBlocked = false;
        updateCursor();
    }

    return retVal;
}

bool GoTable::passMove() {
    //should insert some logic for counting
    if (crtPlayer == WHITE)
        crtPlayer = BLACK;
    else
        crtPlayer = WHITE;

    lastMoveRow = lastMoveCol = -2;

    emit crtPlayerChanged(crtPlayer, players[crtPlayer]);
    cursorBlocked = false;
    update();
    updateCursor();
    if (players[crtPlayer] == PlayerType::AI) {
        QTimer::singleShot(2, this, SLOT(AIPlayNextMove()));
    }
    return true;
}

//change colour of mouse cursor to reflect the current player
void GoTable::updateCursor() {
    if ( (state == GameState::Stopped) || cursorBlocked)
        setCursor(*redCursor);
    else if (crtPlayer == BLACK)
        setCursor(*blackCursor);
    else
        setCursor(*whiteCursor);
}

void GoTable::resetGnuGo() {
    board_size = settings.size;
    if (gnuGoMutex->tryLock() == false) {
        printf("%s - avoided crash with mutex, but there's a logical error\n", __func__);
        gnuGoMutex->lock();
    }
    clear_board();
    gnuGoMutex->unlock();
    //printfGnuGoStruct();
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

bool GoTable::isPosInsideTable(int row, int col) {
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

//TODO - customise to allow restarting sa saved game
void GoTable::launchGame(bool resetTable) {
    game.size = settings.size;
    players[BLACK] = settings.black;
    players[WHITE] = settings.white;
    if (resetTable) {
        crtPlayer = BLACK;
        lastMoveRow = lastMoveCol = -1;
    }
    updateSizes();
    if (useGNUGO) {
        if (resetTable)
            resetGnuGo();
        populateStructFromGnuGo();
    }

    update();
    if (players[crtPlayer] == PlayerType::AI) {
        QTimer::singleShot(5, this, SLOT(AIPlayNextMove()));
    }
}


bool GoTable::AIPlayNextMove() {
    //printf("%s - running on thread %p\n", __func__, QThread::currentThreadId());
    float AIStrength = settings.blackAIStrength;
    if (crtPlayer == WHITE)
        AIStrength = settings.whiteAIStrength;
    AIStrength /= 10;
    aiThread->run_do_genmove(crtPlayer, AIStrength, NULL);
    return false;
}

void GoTable::finish() {
    //TODO - actually here the mutex makes sense; because we can't kill the GnuGo thread and we still want the stop button to have effect
    //maybe show the user a dialog explaining what's hapening.
    if (gnuGoMutex->tryLock() == false) {
        printf("%s - avoided crash with mutex, but there's a logical error\n", __func__);
        gnuGoMutex->lock();
    }

    //TODO - find the GnuGo fancy end computations
    float score = gnugo_estimate_score(NULL, NULL);
    QString winner = "White";
    if (score < 0)
        winner = "Black";
    QString finisher = "White";
    if (crtPlayer == BLACK)
        finisher = "Black";
    QString finalText;
    QTextStream stream(&finalText);
    stream << winner << " won with a score of " << fabs(score) << ".\n" << finisher << " ended the game.";
    stream.flush();

    QFont font;
    int defaultFontSize = font.pixelSize();
    if (defaultFontSize <= 0)
        defaultFontSize = font.pointSize();
    if (defaultFontSize <= 0) {
        printf("%s - error - could not establish a fonst size!\n", __func__);
    }
    int SCALE = 8;
    int diameter = defaultFontSize * SCALE;

    QPixmap winnerPixmap(diameter, diameter);
    winnerPixmap.fill(Qt::transparent);
    QSvgRenderer svgR;
    if (score < 0)
        svgR.load(QString(":/resources/cursorBlack.svg"));
    else
        svgR.load(QString(":/resources/cursorWhite.svg"));
    QPainter bPainter(&winnerPixmap);
    svgR.render(&bPainter);
    crtPlayer = EMPTY;
    update();

    GameEndDialog endDialog(this);
    endDialog.setText(finalText);
    endDialog.setPixmap((winnerPixmap));
    endDialog.setModal(true);
    endDialog.exec();

    gnuGoMutex->unlock();
}

void GoTable::activateEstimatingScore(bool estimate) {
    estimateScore = estimate;
    if (estimate) {
        printf("gnugo: %s, calling gnugo_estimate_score, ts=%s\n", __func__, timer.getTimestampStr().toUtf8().constData());
        if (gnuGoMutex->tryLock() == false) {
            printf("%s - avoided crash with mutex, but there's a logical error\n", __func__);
            gnuGoMutex->lock();
        }
        float score = gnugo_estimate_score(NULL, NULL);
        gnuGoMutex->unlock();
        printf("gnugo: %s, called gnugo_estimate_score, delta=%s\n", __func__, timer.getElapsedStr().toUtf8().constData());
        emit estimateScoreChanged(score);
    }
}

//Move confirmed by pressing the "Confirm" button
void GoTable::userConfirmedMove(int confirmed) {
    printf("%s - confirmed=%d\n", __func__, confirmed);
    const int QTDIALOG_CONFIRMED_CODE = 1;
    if (confirmed == QTDIALOG_CONFIRMED_CODE) {
        placeStone(unconfirmedStoneRow, unconfirmedStoneCol);
    }
    unconfirmedStoneRow = -1;
    unconfirmedStoneCol = -1;
    update();
}

AIThread::AIThread(QMutex *mutex) : mutex(mutex) {

}

bool AIThread::run_do_genmove(int color, float pure_threat_value, int* allowed_moves) {
    printf("%s - color=%d, pure_threat_value=%f\n", __func__, color, pure_threat_value);
    if(running)
        return false;

    running = true;
    p.color = color;
    p.pure_threat_value = pure_threat_value;
    p.allowed_moves = allowed_moves;
    p.value = 0;
    p.resign = 0;
    p.result = 0;
    start();
    return true;
}

void AIThread::run() {
    printf("%s - running on thread %p\n", __func__, QThread::currentThreadId());

    if (mutex->tryLock() == false) {
        printf("%s - avoided crash with mutex, but there's a logical error\n", __func__);
        mutex->lock();
    }
    p.result = do_genmove(p.color, p.pure_threat_value, p.allowed_moves, &p.value, &p.resign);
    mutex->unlock();

    int move = p.result;
    if (move == 0) {
        printf("%s - AI has decided not to move anymore, will compute finals scores.\n", __func__);
        float score = gnugo_estimate_score(NULL, NULL);
        if (score > 0) {
            printf("%s - estimates: white winning by %f\n", __func__, score);
        }
        else {
            printf("%s - estimates: black winning by %f\n", __func__, -score);
        }
        emit AIQuitsGame();
    }
    else {
        QPoint point = GoTable::fromGnuGoPos(move);
        printf("%s - AI has finished\n", __func__);
        emit AIThreadPlaceStone(point.y(), point.x());
    }

    running = false;
}



#include <QElapsedTimer>
#include <QTextStream>
ElapsedTimerWrapper::ElapsedTimerWrapper() {
    t = new QElapsedTimer();
    t->start();
}

ElapsedTimerWrapper::~ElapsedTimerWrapper() {
    delete t;
}

uint64_t ElapsedTimerWrapper::getTimestamp(uint64_t* delta) {
    uint64_t ts = t->elapsed();
    if (delta != NULL)
        *delta = ts - lastTimestamp;
    lastTimestamp = ts;
    return ts;
}

QString ElapsedTimerWrapper::getTimestampStr(QString* delta) {
    uint64_t auxDelta, ts;
    ts = getTimestamp(&auxDelta);
    QTextStream stream;
    if (delta != NULL) {
        stream.setString(delta);
        stream << auxDelta;
    }
    QString retVal;
    stream.setString(&retVal);
    stream << ts;
    stream.flush();
    return retVal;
}

uint64_t ElapsedTimerWrapper::getElapsed() {
    uint64_t ts = t->elapsed();
    uint64_t delta = ts - lastTimestamp;
    lastTimestamp = ts;
    return delta;
}

QString ElapsedTimerWrapper::getElapsedStr() {
    QString retVal;
    QTextStream stream(&retVal);
    stream << getElapsed();
    stream.flush();
    return retVal;
}
