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
#include "engine/board.h" //should probably restrict to the public interface
#include "engine/gnugo.h"
#include "engine/liberty.h"
void init_gnugo(float memory, unsigned int seed);
//void compute_scores(int use_chinese_rules);
float gnugo_estimate_score(float *upper, float *lower);
void value_moves(int color, float pure_threat_value, float our_score,
            int use_thrashing_dragon_heuristics);
}

extern "C" {
int get_sgfmove(SGFProperty *property);
}


#include "GoTable.h"
#include "GameStruct.h"
#include "GameEndDialog.h"
#include "SaveFile.h"

QList<QString> rowNumbering { "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19" };
QList<QString> colNumbering { "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T" };

//From play_test.c
void GoTable::replay_node(SGFNode *node, int color_to_replay, float *replay_score,
                 float *total_score, int* playedMoves, int* crtColour, SGFTree* outTree)
{
    SGFProperty *sgf_prop;  /* iterate over properties of the node */
    SGFProperty *move_prop = NULL; /* remember if we see a move property */
    int color; /* color of move to be made at this node. */

    int old_move; /* The move played in the file. */
    int new_move; /* The move generated by GNU Go. */

    const int BUFSIZE=128;
    char buf[BUFSIZE];

    /* Handle any AB / AW properties, and note presence
   * of move properties.
   */

    for (sgf_prop = node->props; sgf_prop; sgf_prop = sgf_prop->next) {
        switch (sgf_prop->name) {
        case SGFAB:
            /* add black */
            add_stone(get_sgfmove(sgf_prop), BLACK);
            break;
        case SGFAW:
            /* add white */
            add_stone(get_sgfmove(sgf_prop), WHITE);
            break;
        case SGFB:
        case SGFW:
            move_prop = sgf_prop;  /* remember it for later */
            break;
        }
    }

    /* Only generate moves at move nodes. */
    if (!move_prop)
        return;

    old_move = get_sgfmove(move_prop);
    color = (move_prop->name == SGFW) ? WHITE : BLACK;

    if (color == color_to_replay || color_to_replay == GRAY) {
        float new_move_value = 0.0;
        float old_move_value = 0.0;

        /* Get a move from the engine for color. */
        int resign;
        new_move = genmove(color, NULL, &resign);

        /* Pick up the relevant values from the potential_moves[] array. */
        if (new_move != PASS_MOVE)
            new_move_value = potential_moves[new_move];
        if (old_move != PASS_MOVE)
            old_move_value = potential_moves[old_move];

        /* Now report on how well the computer generated the move. */
        if (new_move != old_move || !quiet) {
            mprintf("Move %d (%C): ", movenum + 1, color);

            //curiously enough, adding an opening bracked after "if (resign)" fixed the compilation error...
            if (resign) {
                printf("%s - GNU Go resigns", __func__);
            }
            else {
                mprintf("GNU Go plays %1m ", new_move);
                if (new_move != PASS_MOVE)
                    printf("(%.2f) ", new_move_value);
            }

            mprintf("- Game move %1m ", old_move);
            if (new_move != PASS_MOVE && old_move_value > 0.0)
                printf("(%.2f) ", old_move_value);
            printf("\n");

            *replay_score += new_move_value - old_move_value;
            *total_score += new_move_value;
        }

        if (new_move != old_move) {
            if (resign) {
                printf("%s - GNU Go resigns - Game move %s (%.2f)",
                            __func__, location_to_string(old_move), old_move_value);
            }
            else {
                printf("%s - GNU Go plays %s (%.2f) - Game move %s (%.2f)",
                            __func__, location_to_string(new_move), new_move_value,
                            location_to_string(old_move), old_move_value);
                if (new_move != PASS_MOVE)
                    sgfCircle(node, I(new_move), J(new_move));
            }
        }
        else
            printf("%s - GNU Go plays the same move %s (%.2f)",
                        __func__, location_to_string(new_move), new_move_value);

        sgfAddComment(node, buf);
        sgffile_add_debuginfo(node, 0.0);
    }

    /* Finally, do play the move from the file. */
    QPoint point = GoTable::fromGnuGoPos(old_move);
    printf("%s - move=%d (%d:%d), color=%d\n", __func__, old_move, point.y(), point.x(), color);
    play_move(old_move, color);
    sgftreeAddPlay(outTree, color, point.y(), point.x());
    lastMoveRow = point.y();
    lastMoveCol = point.x();

    (*playedMoves) += 1;
    (*crtColour) = color;
}

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
    changeGameSettings(settings);

    gnuGoMutex = new QMutex;
    aiThread = new AIThread(gnuGoMutex);
    connect(aiThread, SIGNAL(AIThreadPlaceStone(int,int)), this, SLOT(playMove(int,int)));
    connect(aiThread, SIGNAL(AIQuitsGame()), this, SLOT(finish()));


    blockTime = new QTime();


    buildPixmaps(10);
    setCursor(*blackCursor);

    //global from board.h, keep track of it
    sgfTree = (SGFTree*)malloc(sizeof(SGFTree));
    sgfTree->lastnode = NULL;
    sgfTree->root = sgfNewNode();
    if (useGNUGO) {
        init_gnugo(50, 314);
        resetGnuGo(settings.size);
    }

    auxInfo.comment = "test save";
    auxInfo.freeGoVersion = "1000";
    auxInfo.gameDate = "2015-02-19T00:31";
}


GoTable::~GoTable() {
    printf("%s - Implement destructor!\n", __func__);
}

//code outside the constructor because this is executed after the signals of this object are connected
void GoTable::checkForResumeGame() {
    if (loadSaveGameFile(crtGameSfgFName)) {
        state = GameState::Resumed;
    }
    else {
        crtPlayer = BLACK;
        state = GameState::Initial;
    }

    emit crtPlayerChanged(crtPlayer, players[crtPlayer]);
    emit gameStateChanged(state);
}

bool GoTable::loadGame(QString fileName) {
    bool result = loadSaveGameFile(fileName);
    if (result) {
        state = GameState::Resumed;
        emit crtPlayerChanged(crtPlayer, players[crtPlayer]);
        emit gameStateChanged(state);
    }
    return result;
}

bool GoTable::saveGame(QString fileName) {
    //printf("%s, fileName=%s\n", __func__, fileName.toUtf8().constData());
    bool result = SaveFile::writeSave(fileName, sgfTree->root, &this->settings, &auxInfo);
    return result;
}

bool GoTable::loadSaveGameFile(QString fileName) {
    printf("%s, fileName=%s\n", __func__, fileName.toUtf8().constData());
    QFile f(fileName);

    if (!f.exists())
        return false;

    SGFNode* aux = NULL;
    SGameSettings auxSettings;
    SAuxGameInfo auxGameInfo;
    bool success = SaveFile::loadSave(fileName, &aux, &auxSettings, &auxGameInfo);
    //bool success = SaveFile::loadSave()
    if (!success)
        return false;

    sgfFreeNode(sgfTree->root);
    sgfTree->lastnode = NULL;
    sgfTree->root = sgfNewNode();

    changeGameSettings(auxSettings);
    emit pushGameSettings(auxSettings);
    resetGnuGo(auxSettings.size);
    auxInfo = auxGameInfo;


    if (aux == NULL)
        return false;

    bool retVal = false;
    float replayScore = 0.0;
    float totalScore = 0.0;
    int playedMoves = 0;
    SGFNode* node = aux;
    while (node) {
      replay_node(node, EMPTY, &replayScore, &totalScore, &playedMoves, &crtPlayer, sgfTree);
      node = node->child;
    }

    printf("%s - done replaying, replayScore=%f, totalScore=%f\n", __func__, replayScore, totalScore);
    if (playedMoves > 0) {
        populateStructFromGnuGo();
        if (crtPlayer == BLACK)
            crtPlayer = WHITE;
        else
            crtPlayer = BLACK;
        retVal = true;
    }

    sgfFreeNode(aux);

    updateSizes();
    update();
    return retVal;
}

void GoTable::launchGamePressed(SGameSettings newSettings) {
    changeGameSettings(newSettings);

    printf("%s\n", __func__);

    if (state == GameState::Resumed){
        state = GameState::Started;
        if(players[crtPlayer] == PlayerType::AI) {
            QTimer::singleShot(2, this, SLOT(AIPlayNextMove()));
        }
    }
    else if (state == GameState::Initial || state == GameState::Stopped){
        launchGame();
        state = GameState::Started;
    }

    updateCursor();
    emit gameStateChanged(state);
}

void GoTable::changeGameSettings(SGameSettings newSettings) {
    printf("%s\n", __func__);
    settings = newSettings;
    //TODO - check if there are other settings to be written here; should be the only place to change settings
    players[BLACK] = settings.black;
    players[WHITE] = settings.white;
    game.size = settings.size;
    komi = settings.handicap.komi;
    updateSizes();
    update();
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

    //QPointF localPos = ev->localPos();
    //printf("%s - %f, %f -> %d, %d\n", __func__, localPos.ry(), localPos.rx(), pos.y(), pos.x());
}

void GoTable::mouseReleaseEvent(QMouseEvent* ev) {

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

    //printf("%s - x=%d, y=%d, newX=%d, newY=%d, unconfirmedX=%d, unconfirmedY=%d\n",
    //       __func__, pos.x(), pos.y(), newStoneCol, newStoneRow, unconfirmedStoneCol, unconfirmedStoneRow);
    if (pos.x() == newStoneCol && pos.y() == newStoneRow && newStoneCol != -1) {
        if (askPlayConfirmation) {
            //printf("%s - will ask for user confirmation for %d %d\n", __func__, newStoneRow, newStoneCol);
            if (unconfirmedStoneRow == newStoneRow && unconfirmedStoneCol == newStoneCol && acceptDoubleClickConfirmation) {
                //user confirmed by double clicking this
                playMove(pos.y(), pos.x());
                //printf("%s - %d %d confirmed with double click, hiding confirmation window\n", __func__, newStoneRow, newStoneCol);
                newStoneRow = -1;
                newStoneCol = -1;
                unconfirmedStoneRow = -1;
                unconfirmedStoneCol = -1;
                emit askUserConfirmation(false);
            }
            else {
                unconfirmedStoneRow = newStoneRow;
                unconfirmedStoneCol = newStoneCol;
                //printf("%s - show confirmation window for %d %d\n", __func__, newStoneRow, newStoneCol);
                newStoneRow = -1;
                newStoneCol = -1;
                emit askUserConfirmation(true, crtPlayer);
            }
        }
        else {
            playMove(pos.y(), pos.x());
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
    if (players[crtPlayer] == PlayerType::AI && computing) {
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


    QString fontName = "DejaVu Sans";
    int targetHeight = dist * 0.7;
    int auxSmaller, auxLarger;
    int pointSize = getClosestPointSize(fontName, targetHeight, auxSmaller, auxLarger, 1);

    //numbering: from bottom left corner
    QFont font("DejaVu Sans", pointSize);
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
            QColor backColour (0, 0, 64, 220 - 20 * i);
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
            if (platformType() == PlatformType::Android) {
                //smaller screen, we'll show shorter hints
                printable.sprintf("%d", 9 - i);
                pointSize = getClosestPointSize(fontName, drawDiameter, auxSmaller, auxLarger, 1, "0");
            }
            else {
                printable.sprintf("%1.1f", val);
                pointSize = getClosestPointSize(fontName, drawDiameter, auxSmaller, auxLarger, 0, "22.2.");
            }
            //printf("%s - hint move %d %d -> %s\n", __func__, move.y(), move.x(), printable.toUtf8().constData());
            font = QFont(fontName, pointSize);
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


bool GoTable::playMove(int row, int col) {
    printf("placeStone: %d, %d, %d\n", row, col, crtPlayer);
    showHints = false;

    inputBlockingDuration = 0;
    if (players[crtPlayer] == PlayerType::LocalHuman)
        blockTime->start();
    else if (players[crtPlayer] == PlayerType::AI)
        computing = false;

    if (!moveIsLegal(row, col, crtPlayer))
        return false;

    if (state == GameState::Stopped)
        return false;

    if (row == FREEGO_PASS_MOVE) {
        passCount += 1;
        if (passCount >= PASS_COUNT_TO_FINISH) {
            //time to finish the game
            printf("%s - both players have passed consecutively, will end game\n", __func__);
            finish();
			return false;
        }
    }
    else {
        passCount = 0;
    }

    cursorBlocked = true;
    updateCursor();

    bool retVal = false;
    if (useGNUGO) {
        int pos = toGnuGoPos(row, col);
        bool canPlay = is_legal(pos, crtPlayer);

        if (canPlay) {
            //TODO - maybe add wrapper for this call do centralise placing stones
            play_move(pos, crtPlayer);
            retVal = true;
        }
        //printfGnuGoStruct();
        populateStructFromGnuGo();
    }
    else {
        retVal = GamePlaceStone(&game, row, col, crtPlayer);
    }

    if (retVal == false) {
        //refresh and go home
        update();
        return retVal;
    }


    //Here we're sure a move has been played
    sgftreeAddPlay(sgfTree, crtPlayer, row, col);

    SaveFile::writeSave(crtGameSfgFName, sgfTree->root, &this->settings, &auxInfo);

    if (crtPlayer == WHITE)
        crtPlayer = BLACK;
    else
        crtPlayer = WHITE;

    emit crtPlayerChanged(crtPlayer, players[crtPlayer]);
    updateCursor();

    if (row == FREEGO_PASS_MOVE) {
        lastMoveRow = -2;
        lastMoveCol = -2;
    }
    else {
        lastMoveRow = row;
        lastMoveCol = col;
    }


    if (state == GameState::Initial) {
        printf("%s - we just automatically started a new game!\n", __func__);
        state = GameState::Started;
        launchGame(false); //launch game will take of calling AI moves
        emit gameStateChanged(state);
    }
    else {

        if (state == GameState::Resumed) {
            state = GameState::Started;
            emit gameStateChanged(state);
        }
        if (players[crtPlayer] == PlayerType::AI) {
            QTimer::singleShot(2, this, SLOT(AIPlayNextMove()));
        }
    }

    if (players[crtPlayer] != PlayerType::AI) {
        inputBlockingDuration = blockTime->elapsed();
        cursorBlocked = false;
        updateCursor();
    }


    update();

    if (estimateScore) {
        //hack to give GUI time to update
        QTimer::singleShot(20, this, SLOT(computeScoreAndUpdate()));
    }

    return retVal;
}

bool GoTable::passMove() {
    //should insert some logic for counting

    unconfirmedStoneRow = FREEGO_PASS_MOVE;
    emit askUserConfirmation(true, crtPlayer);
    return true;
}

bool GoTable::undoMove() {
    //human-computer: undo twice, computer's and yours
    //human-human: undo once, players will cooperate somehow
    //network-x:no undo for now
    //can't undo on computer's turn, but you can undo after the match finishes
    int count = 0;
    if ((players[crtPlayer] == PlayerType::LocalHuman) /* || (state == GameState::Stopped)*/) {
        if (players[otherColour(crtPlayer)] == PlayerType::AI)
            count = 2;
        else if (players[otherColour(crtPlayer)] == PlayerType::LocalHuman)
            count = 1;
        else
            printf("%s - can't undo against network game\n", __func__);
    }

    int result = undo_move(count);
    if (result == 1) {
        //success: go back in history too
        for(int i = 0; i < count; i++) {
            crtPlayer = otherColour(crtPlayer);
            sgftreeBack(sgfTree);
        }
        SaveFile::writeSave(crtGameSfgFName, sgfTree->root, &settings, &auxInfo);
        populateStructFromGnuGo();
        emit crtPlayerChanged(crtPlayer, players[crtPlayer]);
        showHints = false;
        update();
        updateCursor();
        return true;
    }
    return false;
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

//When failing to compute the score will try later
void GoTable::computeScoreAndUpdate() {
    static int tries = 0;
    const int maxTries = 100;
    bool success = true;
    float score = wrapper_gnugo_estimate_score(NULL, NULL, false, &success);
    if (success) {
        emit estimateScoreChanged(score);
        tries = 0;
    }
    else {
        QTimer::singleShot(100, this, SLOT(computeScoreAndUpdate()));
        tries += 1;
        if (tries > maxTries) {
            printf("%s - failed %d times in a row. Why did we block for so long?", __func__, tries);
        }
    }
    //printf("%s, called gnugo_estimate_score, delta=%s\n", __func__, timer.getElapsedStr().toUtf8().constData());
}

//whenever waitForLock is false also sucess has to be non-null
//TODO - maybe move to AIThread to not block GUI
float GoTable::wrapper_gnugo_estimate_score(float *upper, float *lower, bool waitForLock, bool* success) {
    if (gnuGoMutex->tryLock() == false) {
        if (waitForLock == false) {
            *success = false;
            return -INFINITY;
        }
        gnuGoMutex->lock();
    }
    float score= gnugo_estimate_score(upper, lower);
    gnuGoMutex->unlock();
    return score;
}

void GoTable::resetGnuGo(int newSize) {
    board_size = newSize;
    if (gnuGoMutex->tryLock() == false) {
        printf("%s - avoided crash with mutex, but there's a logical error\n", __func__);
        gnuGoMutex->lock();
    }
    clear_board();
    gnuGoMutex->unlock();
    //printfGnuGoStruct();
}

int GoTable::toGnuGoPos(int row, int col) {
    int pos = (row+1) * 20 + col + 1;
    if (row == FREEGO_PASS_MOVE)
        pos = 0;
    return pos;
}

//TODO - here we're just supposing that that table will stay the same size
QPoint GoTable::fromGnuGoPos(int pos) {
    int row = pos / 20 - 1;
    int col = pos - (row+1)*20 - 1;

    if (pos == 0) {
        col = FREEGO_PASS_MOVE;
        row = FREEGO_PASS_MOVE;
    }
    return QPoint(col, row);
}

void GoTable::printfGnuGoStruct() {
    for (int i = 0; i < 21; i++) {
        for (int j = 0; j < 20; j++)
            printf("%d", board[i*20+j]);
        printf("\n");
    }
}

bool GoTable::moveIsLegal(int row, int col, int colour) {
    int pos = toGnuGoPos(row, col);
    if (row == FREEGO_PASS_MOVE)
        pos = PASS_MOVE;
    return (is_legal(pos, colour));
}

int GoTable::populateStructFromGnuGo() {
    for (int i = 0; i < game.size; i++) {
        for(int j = 0; j < game.size; j++)
            game.state[i][j] = board[toGnuGoPos(i, j)];
    }
    return 0;
}

//TODO - customise to allow restarting saved game
void GoTable::launchGame(bool resetTable) {
    game.size = settings.size;
    players[BLACK] = settings.black;
    players[WHITE] = settings.white;
    showHints = false;
    if (resetTable) {
        crtPlayer = BLACK;
        lastMoveRow = lastMoveCol = -1;
    }
    emit crtPlayerChanged(crtPlayer, players[crtPlayer]);
    updateSizes();
    if (useGNUGO) {
        if (resetTable)
            resetGnuGo(settings.size);
        populateStructFromGnuGo();
    }

    update();
    if (players[crtPlayer] == PlayerType::AI) {
        QTimer::singleShot(2, this, SLOT(AIPlayNextMove()));
    }
}


bool GoTable::AIPlayNextMove() {
    computing = true;
    update();
    //printf("%s - running on thread %p\n", __func__, QThread::currentThreadId());
    int AIStrength = settings.blackAIStrength;
    if (crtPlayer == WHITE)
        AIStrength = settings.whiteAIStrength;
    aiThread->run_genmove(crtPlayer, AIStrength);
    return false;
}

void GoTable::finish() {
    //float score = wrapper_gnugo_estimate_score(NULL, NULL);

    //GnuGo crashes if we attempt to aftermath_compute_score on an empty board;
    float blackScore = 0.0;

    if (gnuGoMutex->tryLock() == false) {
        printf("%s - avoided crash with mutex, but there's a logical error\n", __func__);
        gnuGoMutex->lock();
    }

    if (countStones(&game) > 0)
        blackScore = aftermath_compute_score(BLACK, NULL);


    //TODO - actually here the mutex makes sense; because we can't kill the GnuGo thread and we still want the stop button to have effect
    //maybe show the user a dialog explaining what's hapening.

    //TODO - find the GnuGo fancy end computations
    QString winner = "White";
    if (blackScore > 0.0)
        winner = "Black";
    else if (blackScore == 0.0) {
        winner = "Tie";
    }
    QString finisher = "White";
    if (crtPlayer == BLACK)
        finisher = "Black";
    QString finalText;
    QTextStream stream(&finalText);
    if (blackScore == 0.0) {
        stream << "Game ended with a tie.";
    }
    else {
        stream << winner << " won with a score of " << fabs(blackScore) << ".\n" << finisher << " ended the game.";
    }
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
    if (blackScore > 0.0)
        svgR.load(QString(":/resources/cursorBlack.svg"));
    else if (blackScore < 0.0)
        svgR.load(QString(":/resources/cursorWhite.svg"));
    else
        svgR.load(QString(":/resources/cursorBlackWhite.svg"));

    QPainter bPainter(&winnerPixmap);
    svgR.render(&bPainter);
    crtPlayer = EMPTY;
    showHints = false;
    update();

    //File saving stuff
    QString oldSgfFName = crtGameSfgFName + ".old";
    QFile file(oldSgfFName);
    if (file.exists())
        file.remove();

    file.setFileName(crtGameSfgFName);
    file.rename(oldSgfFName);

    sgfFreeNode(sgfTree->root);
    sgfTree->lastnode = NULL;
    sgfTree->root = sgfNewNode();


    GameEndDialog endDialog(this);
    endDialog.setText(finalText);
    endDialog.setPixmap((winnerPixmap));
    endDialog.setModal(true);
    endDialog.exec();

    state = GameState::Stopped;
    emit gameStateChanged(state);

    gnuGoMutex->unlock();
}

void GoTable::activateEstimatingScore(bool estimate) {
    estimateScore = estimate;
    if (estimate) {
        QTimer::singleShot(20, this, SLOT(computeScoreAndUpdate()));
    }
}

//Move confirmed by pressing the "Confirm" button
void GoTable::userConfirmedMove(int confirmed) {
    printf("%s - confirmed=%d\n", __func__, confirmed);
    const int QTDIALOG_CONFIRMED_CODE = 1;
    if (confirmed == QTDIALOG_CONFIRMED_CODE) {
        playMove(unconfirmedStoneRow, unconfirmedStoneCol);
    }
    unconfirmedStoneRow = -1;
    unconfirmedStoneCol = -1;
    update();
}

void GoTable::showPlayHints() {
    showHints = !showHints;
    if (showHints) {
        aiThread->run_value_moves(crtPlayer);
    }
    update();
}

AIThread::AIThread(QMutex *mutex) : mutex(mutex) {

}

bool AIThread::run_genmove(int color, int AIStrength) {
    printf("%s - color=%d\n", __func__, color);
    if(running)
        return false;

    running = true;
    p.operation = OpType::do_genmove;
    p.color = color;
    p.strength = AIStrength;
    p.value = 0;
    p.resign = 0;
    p.result = 0;
    start();
    return true;
}

bool AIThread::run_gnugo_estimate_score() {
    if(running)
        return false;
    running = true;
    p.operation = OpType::gnugo_estimate_score;
    start();
    return true;
}

void AIThread::run_value_moves(int colour) {
    if (mutex->tryLock() == false) {
        printf("%s - avoided crash with mutex, but there's a logical error\n", __func__);
        mutex->lock();
    }

    //value_moves(colour, 0.0, 0.0, 1);
    //run genmove to fill in best_move_values
    set_level(2);
    int val = genmove(colour, NULL, NULL);
    Q_UNUSED(val);
    mutex->unlock();;
}

void AIThread::run() {
    printf("%s - running on thread %p\n", __func__, QThread::currentThreadId());

    if (mutex->tryLock() == false) {
        printf("%s - avoided crash with mutex, but there's a logical error\n", __func__);
        mutex->lock();
    }
    set_level(p.strength);
    p.result = genmove(p.color, &p.move_value, &p.resign);
    mutex->unlock();

    int move = p.result;
    if (p.resign) {
        printf("%s - AI has decided to resign, will compute finals scores.\n", __func__);
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
        printf("%s - AI has finished, move of value=%f, at %d, %d\n", __func__, p.move_value, point.y(), point.x());
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

#include <limits>
int GoTable::getClosestPointSize(QString fontName, int targetSize, int& nextSmaller, int& nextLarger, int direction, QString text) {
    //haha - clumsy c++ syntax :)))
    int closestSize = std::numeric_limits<int>::max();
    int nextSmallerSize = std::numeric_limits<int>::max();
    int nextLargerSize = std::numeric_limits<int>::max();
    int closest = -1;
    nextSmaller = -1;
    nextLarger = -1;
    int pointSize = 1;
    int maxPointSize = 96;
    while(pointSize <= maxPointSize) {
        QFontMetrics fontMetrics = QFontMetrics(QFont(fontName, pointSize));
        int crtSize = 0;
        if (direction == 0) {
            crtSize = fontMetrics.width(text);
        }
        else {
            crtSize = fontMetrics.height();
        }
        //printf("%s - pointSize=%d. Results in height=%d, we need height=%d\n",
        //       __func__, pointSize, crtSize, targetSize);
        if (abs(crtSize - targetSize) < abs(crtSize - closestSize)) {
            closest = pointSize;
            closestSize = crtSize;
        }
        if ( (crtSize < targetSize) && (abs(crtSize - targetSize) < abs(crtSize - nextSmallerSize)) ) {
            nextSmaller = pointSize;
            nextSmallerSize = crtSize;
        }
        if ( (crtSize > targetSize) && (abs(crtSize - targetSize) < abs(crtSize - nextLargerSize)) ) {
            nextLarger = pointSize;
            nextLargerSize = crtSize;
        }
        if ((closestSize != std::numeric_limits<int>::max()) &&
            (nextSmallerSize != std::numeric_limits<int>::max()) &&
            (nextLargerSize != std::numeric_limits<int>::max()))
            break;
        pointSize += 1;
    }
    //printf("%s, closest:%d:%d, smaller:%d:%d, larger:%d:%d\n", __func__,
    //       closest, closestSize, nextSmaller, nextSmallerSize, nextLarger, nextLargerSize);
    return closest;
}

