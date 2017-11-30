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
#include <QLabel>
#include <QtMultimedia/QSound>

//temporary for debug.
#include <QJsonDocument>

#include <cmath>


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
#include "BusyDialog.h"
#include "Utils.h"
#include "Logger.h"
//likely temporary
#include "SettingsWidget.h"


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

    setMinimumWidth(250);
    setMinimumHeight(250);

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

    askPlayConfirmation = true;
    #if defined(Q_OS_ANDROID)
    askPlayConfirmation = true;
    #endif

    gnuGoMutex = new QMutex;
    aiThread = new AIThread(gnuGoMutex);
    connect(aiThread, SIGNAL(AIThreadPlaceStone(int,int)), this, SLOT(playMove(int,int)));
    connect(aiThread, SIGNAL(AIQuitsGame(bool)), this, SLOT(finish(bool)));

    game.size = gameSettings.size;
    players[EMPTY] = PlayerType::None;
    changeGameSettings(gameSettings); //an idiotic move of copying gameSettings over gameSettings :D


    blockTime = new QTime();


    buildPixmaps(10);
    setCursor(*blackCursor);

    //global from board.h, keep track of it
    sgfTree = (SGFTree*)malloc(sizeof(SGFTree));
    sgfTree->lastnode = NULL;
    sgfTree->root = sgfNewNode();
    if (useGNUGO) {
        init_gnugo(50, 314);
        resetGnuGo(gameSettings.size);
    }

    //TODO - fix these joke data
    auxInfo.comment = "test save";
    auxInfo.freeGoVersion = "1000";
    auxInfo.gameDate = "2015-02-19T00:31";

    programSettings = Settings::getProgramSettings();
}


GoTable::~GoTable() {
    printf("%s - Implement destructor!\n", __func__);
    delete gnuGoMutex;
}

//This code is outside the constructor because this is executed after the signals of this object are connected
void GoTable::checkForResumeGame() {
    if (loadGame(crtGameSfgFName)) {
        state = GameState::Resumed;
    }
    else {
        crtPlayer = BLACK;
        state = GameState::Initial;
        resetGnuGo(gameSettings.size);
        populateStructFromGnuGo();
    }

    emit crtPlayerChanged(crtPlayer, players[crtPlayer], players[otherColour(crtPlayer)]);
    emit gameStateChanged(state);
}

GameState GoTable::getGameState() const {
    return state;
}

void GoTable::getPlayersState(int& crt, PlayerType& crtType, PlayerType& opponentType) const {
    crt = this->crtPlayer;
    crtType = players[crtPlayer];
    opponentType = players[otherColour(crtPlayer)];
}

/**
 * @brief GoTable::setSecondPlayerToNetwork hack function to instruct that the second player is now of type network.
 */
void GoTable::setSecondPlayerToNetwork() {
    Logger::log(QString("%1").arg(__func__));
    if (gameSettings.white == PlayerType::LocalHuman) {
        gameSettings.black = PlayerType::Network;
    }
    else if (gameSettings.black == PlayerType::LocalHuman) {
        gameSettings.white = PlayerType::Network;
    }
    emit pushGameSettings(gameSettings);
    update();
}

void GoTable::changeProgramSettings() {
    update();
}

SGameSettings* GoTable::getGameSettingsPointer() {
    return &gameSettings;
}

bool GoTable::saveGame(QJsonObject& json) {
    bool result = SaveFile::writeSave(json, sgfTree->root, &this->gameSettings, &auxInfo);
    return result;
}

bool GoTable::saveGame(QString fileName) {
    Logger::log(QString("%1, fileName=%2").arg(__func__).arg(fileName));
    bool result = SaveFile::writeSave(fileName, sgfTree->root, &this->gameSettings, &auxInfo);
    return result;
}

/**
 * @brief GoTable::saveGameForRemote serialise the game for playing with a remote (Network) player.
 * @param json
 * @return
 */
bool GoTable::saveGameForRemote(QJsonObject& json) {
    bool result = SaveFile::writeSaveForRemote(json, sgfTree->root, &this->gameSettings, &auxInfo);
    return result;
}

bool GoTable::loadGame(SGFNode* aux, SGameSettings auxSettings, SAuxGameInfo auxGameInfo) {
    sgfFreeNode(sgfTree->root);
    sgfTree->lastnode = NULL;
    sgfTree->root = sgfNewNode();

    changeGameSettings(auxSettings);
    emit pushGameSettings(auxSettings);
    resetGnuGo(auxSettings.size);
    auxInfo = auxGameInfo;


    if (aux == NULL) {
        Logger::log("aux == NULL", Logger::ERR);
        return false;
    }

    bool retVal = false;
    float replayScore = 0.0;
    float totalScore = 0.0;
    int playedMoves = 0;
    SGFNode* node = aux;
    while (node) {
      replay_node(node, EMPTY, &replayScore, &totalScore, &playedMoves, &crtPlayer, sgfTree);
      node = node->child;
    }

    Logger::log(QString("%1 - done replaying, replayScore=%2, totalScore=%3").arg(__func__).arg(replayScore).arg(totalScore));
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

bool GoTable::loadGame(const QString fileName) {
    Logger::log(QString("%1, fileName=%2").arg(__func__).arg(fileName));
    QFile f(fileName);
    if (!f.exists() || !f.open(QIODevice::ReadOnly))
        return false;

    QByteArray data = f.readAll();

    SGFNode* aux = NULL;
    SGameSettings auxSettings;
    SAuxGameInfo auxGameInfo;

    bool success = SaveFile::loadSave(data, &aux, &auxSettings, &auxGameInfo);
    if (!success)
        return false;
    loadGame(aux, auxSettings, auxGameInfo);

    return success;
}

bool GoTable::loadGameFromRemote(const QJsonObject &json) {
    SGFNode* aux = NULL;
    SGameSettings auxSettings;
    SAuxGameInfo auxGameInfo;
    bool success = SaveFile::loadSaveFromRemote(json, &aux, &auxSettings, &auxGameInfo);
    if (!success) {
        Logger::log(QString("%1 - could not load remote save: %2").arg(__func__).arg(QJsonDocument(json).toJson().constData()), Logger::ERR);
        return false;
    }

    success = loadGame(aux, auxSettings, auxGameInfo);
    if (!success) {
        Logger::log(QString("%1 - loading failed. Investigate").arg(__func__));
    }

    if (success) {
        state = GameState::Resumed;
        emit crtPlayerChanged(crtPlayer, players[crtPlayer], players[otherColour(crtPlayer)]);
        emit gameStateChanged(state);
    }

    return success;
}

bool GoTable::loadGameAndStart(const QString fileName) {
    bool success = loadGame(fileName);
    if (success) {
        state = GameState::Resumed;
        emit crtPlayerChanged(crtPlayer, players[crtPlayer], players[otherColour(crtPlayer)]);
        emit gameStateChanged(state);
    }
    return success;
}

void GoTable::launchGamePressed(SGameSettings newSettings) {
    printf("%s\n", __func__);

    if (state == GameState::Resumed) {
        state = GameState::Started;
        if(players[crtPlayer] == PlayerType::AI) {
            QTimer::singleShot(2, this, SLOT(AIPlayNextMove()));
        }
    }
    else if (state == GameState::Initial || state == GameState::Stopped) {
        changeGameSettings(newSettings);
        launchGame();
        state = GameState::Started;
    }

    updateCursor();
    emit gameStateChanged(state);
}

void GoTable::changeGameSettings(SGameSettings newSettings) {
    printf("%s\n", __func__);
    //TODO - check if there are other settings to be written here; should be the only place to change settings
    players[BLACK] = newSettings.black;
    players[WHITE] = newSettings.white;
    game.size = newSettings.size;
    if (gameSettings.size != newSettings.size) {
        printf("%s - settings.size=%d, newSettings.size=%d\n", __func__, gameSettings.size, newSettings.size);
        resetGnuGo(game.size);
    }
    if (newSettings.handicap.handicap != gameSettings.handicap.handicap) {
        resetGnuGo((game.size));
        insertDefaultHandicap(gameSettings.handicap.handicap);
    }
    komi = gameSettings.handicap.komi;
    gameSettings = newSettings;
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
        emit highlightChanged(highlightRow, highlightCol);
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

    QPoint pos = mouseToGameCoordinates(ev);
    highlightRow = pos.y();
    highlightCol = pos.x();
    emit highlightChanged(highlightRow, highlightCol);

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
}

void GoTable::mouseReleaseEvent(QMouseEvent* ev) {
    lastInputTimestamp = ev->timestamp();

    if (players[crtPlayer] != PlayerType::LocalHuman) {
        printf("%s - return because crtPlayer is not localHuman\n", __func__);
        return;
    }

    QPoint pos = mouseToGameCoordinates(ev);

    if (pos.x() == newStoneCol && pos.y() == newStoneRow && newStoneCol != -1) {
        if (askPlayConfirmation) {
            if (unconfirmedStoneRow == newStoneRow && unconfirmedStoneCol == newStoneCol && acceptDoubleClickConfirmation) {
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
    //setMask(QRegion(dist/2, dist/2, (game.size + 1)*dist, (game.size + 1)*dist));
}

void GoTable::updateSizes() {
    int tableSize = width(); //compute and enforce correctly
    if (height() < tableSize)
        tableSize = height();
    dist = gridDist(tableSize, game.size);
    diameter = dist * stoneDiameter();

    buildPixmaps(diameter);
    updateCursor();
}

float GoTable::gridDist(float tableSize, int gameSize) {
    float ret = tableSize / (gameSize + 1);
    return ret;
}

float GoTable::stoneDiameter() {
    return 0.95;
}

/**
 * @brief GoTable::playMove - try to play a move, all GUI conditions have been fullfilled, now check the logic ones.
 * Special case for the networked game: we play, and perform and undo in case the move is not accepted
 */
bool GoTable::playMove(int row, int col) {
    Logger::log(QString("%1: %2, %3. Player %4 of type %5").arg(__func__).arg(row).arg(col).arg(crtPlayer).arg(playerTypeMap.left.at(players[crtPlayer])));
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
            Logger::log(QString("%1 - both players have passed consecutively, will end game.").arg(__func__));
            finish(false);
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
        else {
            Logger::log(QString("Move not legal: %1 %2. Plyer %3 of type %4").arg(row).arg(col).arg(crtPlayer).arg(playerTypeMap.left.at(players[crtPlayer])));
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

    if (programSettings->soundsVolume > 0) {
        QSound::play(QString(":/resources/sounds/click.wav"));
    }

    //Aici - with a remote player we need to spin and wait for confirmation later in the process


    //Here we're sure a move has been played
    if (state == GameState::Initial) {
        sgfFreeNode(sgfTree->root);
        sgfTree->lastnode = NULL;
        sgfTree->root = sgfNewNode();
    }

    sgftreeAddPlay(sgfTree, crtPlayer, row, col);
    SaveFile::writeSave(crtGameSfgFName, sgfTree->root, &this->gameSettings, &auxInfo);

    crtPlayer = otherColour(crtPlayer);

    if (state == GameState::Initial) {
        printf("%s - we just automatically started a new game!\n", __func__);
        state = GameState::Started;
        launchGame(false);
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

    updateCursor();

    if (row == FREEGO_PASS_MOVE) {
        lastMoveRow = lastMoveCol = -2;
    }
    else {
        lastMoveRow = row;
        lastMoveCol = col;
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

    emit crtPlayerChanged(crtPlayer, players[crtPlayer], players[otherColour(crtPlayer)]);
    Logger::log(QString("movePlayed: %1 %2").arg(row).arg(col));
    Logger::log(QString("Turn changed to: crtPlayer: %1, crtType: %2, otherType: %3").arg(crtPlayer).arg(playerTypeMap.left.at(players[crtPlayer])).arg(players[otherColour(crtPlayer)]));
    emit movePlayed(row, col);
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
        SaveFile::writeSave(crtGameSfgFName, sgfTree->root, &gameSettings, &auxInfo);
        populateStructFromGnuGo();
        emit crtPlayerChanged(crtPlayer, players[crtPlayer], players[otherColour(crtPlayer)]);
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

/*
 * @param resetTable - the table won't be cleaned-up, this allow launching a game with a stone move.
 */
void GoTable::launchGame(bool resetTable) {
    sgfFreeNode(sgfTree->root);
    sgfTree->lastnode = NULL;
    sgfTree->root = sgfNewNode();

    game.size = gameSettings.size;
    players[BLACK] = gameSettings.black;
    players[WHITE] = gameSettings.white;
    showHints = false;
    if (resetTable) {
        if (gameSettings.handicap.handicap && gameSettings.handicap.handicapPlacementFree == false)
            crtPlayer = WHITE;
        else
            crtPlayer = BLACK;
        lastMoveRow = lastMoveCol = -1;
    }
    emit crtPlayerChanged(crtPlayer, players[crtPlayer], players[otherColour(crtPlayer)]);
    updateSizes();
    if (useGNUGO) {
        if (resetTable) {
            resetGnuGo(gameSettings.size);
            if (gameSettings.handicap.handicapPlacementFree == false)
                insertDefaultHandicap(gameSettings.handicap.handicap);
        }
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
    int AIStrength = gameSettings.blackAIStrength;
    if (crtPlayer == WHITE)
        AIStrength = gameSettings.whiteAIStrength;
    aiThread->run_genmove(crtPlayer, AIStrength);
    return false;
}

void GoTable::finish(bool finishByResign) {
    float approximateZero = 0.001;
    float score = 0.0;
    int stoneCount = countStones(&game);

    if (gnuGoMutex->tryLock() == false) {
        Logger::log(QString("%1 - avoided crash with mutex, but there's a logical error").arg(__func__), Logger::ERR);
        gnuGoMutex->lock();
    }

    bool showEstimateScore = false;

    //if the game was played quite a bit before quitting, we show the winner but also a score estimate
    if (stoneCount  > game.size * game.size / 2) {
        showEstimateScore = true;
        Logger::log(QString("%1 - stoneCount=%2, will force estimating a score").arg(__func__).arg(stoneCount), Logger::DBG);
    }

    BusyDialog busyDialog(this);
    if (finishByResign == false) {
        printf("%s - finished normally. Computing final score.\n", __func__);
        busyDialog.setText("Computing final score");
        busyDialog.show();
        qApp->processEvents();
        score = aftermath_compute_score(BLACK, NULL);
    }
    else if (showEstimateScore){
        printf("%s - finished by resign. Estimating a score.\n", __func__);
        busyDialog.setText(colourName(otherColour(crtPlayer)) + " won by " +
                           colourName(crtPlayer) + "'s resignation.\n Computing a score estimate.");
        busyDialog.show();
        qApp->processEvents();
        score = gnugo_estimate_score(NULL, NULL);
    }
    busyDialog.hide();

    //TODO - actually here the mutex makes sense; because we can't kill the GnuGo thread and we still want the stop button to have effect
    //maybe show the user a dialog explaining what's hapening.

    //TODO - find the GnuGo fancy end computations
    int winner = EMPTY;
    if (finishByResign) {
        winner = otherColour(crtPlayer);
    }
    else {
        winner = WHITE;
        if (score < -approximateZero) {
            winner = BLACK;
        }
        else if (fabs(score) < approximateZero) {
            winner = EMPTY;
        }
    }

    QString finalText;
    QTextStream stream(&finalText);
    if (finishByResign) {
        stream << "<h3>" << colourName(winner) << " won.</h3>" +
                  colourName(otherColour(winner)) << " resigned.\n<br>";
        if (showEstimateScore) {
            int estimatedWinner = WHITE;
            if (score < -approximateZero) {
                estimatedWinner = BLACK;
            }

            if (fabs(score) < approximateZero) {
                estimatedWinner = EMPTY;
                stream << "Estimate: tie.";
            }
            else {
                stream << "Estimate: " + colourName(estimatedWinner) + " winning with a score of " +
                          QString::number(fabs(score), 'g', 2) + ".";
            }
        }
    }
    else {
        stream << "<h4>" << colourName(winner) << " won. Score: " <<
                  QString::number(fabs(score), 'g', 2) << ".</h4>";
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
    if (winner == BLACK)
        svgR.load(QString(":/resources/cursorBlack.svg"));
    else if (winner == WHITE)
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

void GoTable::insertDefaultHandicap(int newHandicap) {
    //http://en.wikipedia.org/wiki/Go_handicaps#Fixed_placement
    printf("%s - newHandicap=%d", __func__, newHandicap);
    place_fixed_handicap(newHandicap);
    populateStructFromGnuGo();
    //register all moves, since this is not done by GnuGo
    for(int row = 0; row < game.size; row++) {
        for(int col = 0; col < game.size; col++) {
            if (game.state[row][col] != 0) {
                sgftreeAddPlay(sgfTree, game.state[row][col], row, col);
            }
        }
    }

    //GnuGo may have decided the handicap is inappropriate for the table size
    if (newHandicap != handicap) {
        gameSettings.handicap.handicap = handicap;
        emit pushGameSettings(gameSettings);
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
        Logger::log(QString("%1 - AI has decided to resign, will compute finals scores.").arg(__func__));
        float score = gnugo_estimate_score(NULL, NULL);
        if (score > 0) {
            printf("%s - estimates: white winning by %f\n", __func__, score);
        }
        else {
            printf("%s - estimates: black winning by %f\n", __func__, -score);
        }
        emit AIQuitsGame(true);
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

