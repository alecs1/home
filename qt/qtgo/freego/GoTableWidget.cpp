#include "GoTableWidget.h"

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

#include "GameStruct.h"
#include "GameEndDialog.h"
#include "SaveFile.h"
#include "BusyDialog.h"
#include "Utils.h"
#include "Logger.h"
//likely temporary
#include "SettingsWidget.h"


GoTableWidget::GoTableWidget(QWidget *parent) :
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

    game.size = gameSettings.size;
    changeGameSettings(gameSettings); //an idiotic move of copying gameSettings over gameSettings :D

    buildPixmaps(10);
    setCursor(*blackCursor);

    programSettings = Settings::getProgramSettings();
}


GoTableWidget::~GoTableWidget() {
    Logger::log(QString("%1 - Implement destructor!").arg(__func__));
}

//This code is outside the constructor because this is executed after the signals of this object are connected
void GoTableWidget::checkForResumeGame() {
    GoTable::checkForResumeGame();

    emit crtPlayerChanged(crtPlayer, players[crtPlayer], players[otherColour(crtPlayer)]);
    emit gameStateChanged(state);
}

/**
 * @brief GoTableWidget::setSecondPlayerToNetwork hack function to instruct that the second player is now of type network.
 */
void GoTableWidget::setSecondPlayerToNetwork() {
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

void GoTableWidget::changeProgramSettings() {
    update();
}

bool GoTableWidget::loadGameAndStart(const QString fileName) {
    bool success = loadGame(fileName);
    if (success) {
        state = GameState::Resumed;
        emit crtPlayerChanged(crtPlayer, players[crtPlayer], players[otherColour(crtPlayer)]);
        emit gameStateChanged(state);
    }
    return success;
}

bool GoTableWidget::loadGameFromRemote(const QJsonObject &json) {
    bool success = GoTable::loadGameFromRemote(json);
    if (success) {
        emit crtPlayerChanged(crtPlayer, players[crtPlayer], players[otherColour(crtPlayer)]);
        emit gameStateChanged(state);
    }
}

void GoTableWidget::launchGamePressed(SGameSettings newSettings) {
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

void GoTableWidget::changeGameSettings(const SGameSettings& newSettings) {
    GoTable::changeGameSettings(newSettings);
    updateSizes();
    update();
}

void GoTableWidget::mouseMoveEvent(QMouseEvent* ev) {
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

void GoTableWidget::mousePressEvent(QMouseEvent* ev) {
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

void GoTableWidget::mouseReleaseEvent(QMouseEvent* ev) {
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

QPoint GoTableWidget::mouseToGameCoordinates(QMouseEvent* ev) {
    QPointF localPos = ev->localPos();

    //compute the closest intersection
    int row = localPos.ry() / dist - 0.5;
    int col = localPos.rx() / dist - 0.5;
    if ( row >= 0 && row < game.size && col >= 0 && col < game.size)
        return QPoint(col, row);
    else
        return QPoint(-1, -1);
}

void GoTableWidget::resizeEvent(QResizeEvent* event) {
    Q_UNUSED(event);
    updateSizes();
    //setMask(QRegion(dist/2, dist/2, (game.size + 1)*dist, (game.size + 1)*dist));
}

void GoTableWidget::updateSizes() {
    int tableSize = width(); //compute and enforce correctly
    if (height() < tableSize)
        tableSize = height();
    dist = gridDist(tableSize, game.size);
    diameter = dist * stoneDiameter();

    buildPixmaps(diameter);
    updateCursor();
}

float GoTableWidget::gridDist(float tableSize, int gameSize) {
    float ret = tableSize / (gameSize + 1);
    return ret;
}

float GoTableWidget::stoneDiameter() {
    return 0.95;
}

/**
 * @brief GoTableWidget::playMove - try to play a move, all GUI conditions have been fullfilled, now check the logic ones.
 * Special case for the networked game: we play, and perform and undo in case the move is not accepted
 */
bool GoTableWidget::playMove(const int row, const int col) {
    cursorBlocked = true;
    bool retVal = GoTable::playMove(row, col);

    if (retVal) {
        if (programSettings->soundsVolume > 0) {
            QSound::play(QString(":/resources/sounds/click.wav"));
        }
    }

    cursorBlocked = false;
    updateCursor();
    update();

    emit gameStateChanged(state);

    if (estimateScore) {
        //hack to give GUI time to update
        QTimer::singleShot(20, this, SLOT(computeScoreAndUpdate()));
    }

    emit crtPlayerChanged(crtPlayer, players[crtPlayer], players[otherColour(crtPlayer)]);
    emit movePlayed(row, col);
    return retVal;
}

bool GoTableWidget::passMove() {
    //should insert some logic for counting
    GoTable::passMove();
    unconfirmedStoneRow = FREEGO_PASS_MOVE;
    emit askUserConfirmation(true, crtPlayer);
    return true;
}

bool GoTableWidget::undoMove() {
    bool retVal = GoTable::undoMove();
    emit crtPlayerChanged(crtPlayer, players[crtPlayer], players[otherColour(crtPlayer)]);
    updateCursor();
    update();
    return retVal;
}

//change colour of mouse cursor to reflect the current player
void GoTableWidget::updateCursor() {
    if ( (state == GameState::Stopped) || cursorBlocked)
        setCursor(*redCursor);
    else if (crtPlayer == BLACK)
        setCursor(*blackCursor);
    else
        setCursor(*whiteCursor);
}

//When failing to compute the score will try later
void GoTableWidget::computeScoreAndUpdate() {
    static int tries = 0;
    const int maxTries = 100;
    bool success = true;
    float score = GoTable::wrapper_gnugo_estimate_score(NULL, NULL, false, &success);
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

/*
 * @param resetTable - the table won't be cleaned-up, this allow launching a game with a stone move.
 */
void GoTableWidget::launchGame(bool resetTable) {
    GoTable::launchGame(resetTable);
    emit crtPlayerChanged(crtPlayer, players[crtPlayer], players[otherColour(crtPlayer)]);
    updateSizes();
    update();
    if (players[crtPlayer] == PlayerType::AI) {
        QTimer::singleShot(2, this, SLOT(AIPlayNextMove()));
    }
}


bool GoTableWidget::AIPlayNextMove() {
    GoTable::AIPlayNextMove();
    update();
    return false;
}

void GoTableWidget::finish(bool finishByResign) {
    bool showEstimateScore = false;
    float approximateZero = 0.001;

    //if the game was played quite a bit before quitting, we show the winner but also a score estimate
    int stoneCount = countStones(&game);
    if (stoneCount  > game.size * game.size / 2) {
        showEstimateScore = true;
        Logger::log(QString("%1 - stoneCount=%2, will force estimating a score").arg(__func__).arg(stoneCount), Logger::DBG);
    }


    BusyDialog busyDialog(this);
    float score = GoTable::finish(finishByResign);

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


    if (finishByResign == false) {
        printf("%s - finished normally. Computing final score.\n", __func__);
        busyDialog.setText("Computing final score");
        busyDialog.show();
        qApp->processEvents();
        score = wrapper_aftermath_compute_score();
    }
    else if (showEstimateScore){
        printf("%s - finished by resign. Estimating a score.\n", __func__);
        busyDialog.setText(colourName(otherColour(crtPlayer)) + " won by " +
                           colourName(crtPlayer) + "'s resignation.\n Computing a score estimate.");
        busyDialog.show();
        qApp->processEvents();
        bool success = false;
        score = wrapper_gnugo_estimate_score(NULL, NULL, false, &success);
    }
    busyDialog.hide();

    //TODO - actually here the mutex makes sense; because we can't kill the GnuGo thread and we still want the stop button to have effect
    //maybe show the user a dialog explaining what's hapening.


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



    GameEndDialog endDialog(this);
    endDialog.setText(finalText);
    endDialog.setPixmap((winnerPixmap));
    endDialog.setModal(true);
    endDialog.exec();

    state = GameState::Stopped;
    emit gameStateChanged(state);
}

void GoTableWidget::activateEstimatingScore(bool estimate) {
    estimateScore = estimate;
    if (estimate) {
        QTimer::singleShot(20, this, SLOT(computeScoreAndUpdate()));
    }
}

//Move confirmed by pressing the "Confirm" button
void GoTableWidget::userConfirmedMove(int confirmed) {
    printf("%s - confirmed=%d\n", __func__, confirmed);
    const int QTDIALOG_CONFIRMED_CODE = 1;
    if (confirmed == QTDIALOG_CONFIRMED_CODE) {
        playMove(unconfirmedStoneRow, unconfirmedStoneCol);
    }
    unconfirmedStoneRow = -1;
    unconfirmedStoneCol = -1;
    update();
}

void GoTableWidget::showPlayHints() {
    showHints = !showHints;
    if (showHints) {
        aiThread->run_value_moves(crtPlayer);
    }
    update();
}

void GoTableWidget::insertDefaultHandicap(int newHandicap) {
    int computedHandicap = GoTable::insertDefaultHandicap(newHandicap);
    if (computedHandicap != newHandicap) {
        emit pushGameSettings(gameSettings);
    }
    update();
}

//#include <QElapsedTimer>
//#include <QTextStream>
//ElapsedTimerWrapper::ElapsedTimerWrapper() {
//    t = new QElapsedTimer();
//    t->start();
//}

//ElapsedTimerWrapper::~ElapsedTimerWrapper() {
//    delete t;
//}

//uint64_t ElapsedTimerWrapper::getTimestamp(uint64_t* delta) {
//    uint64_t ts = t->elapsed();
//    if (delta != NULL)
//        *delta = ts - lastTimestamp;
//    lastTimestamp = ts;
//    return ts;
//}

//QString ElapsedTimerWrapper::getTimestampStr(QString* delta) {
//    uint64_t auxDelta, ts;
//    ts = getTimestamp(&auxDelta);
//    QTextStream stream;
//    if (delta != NULL) {
//        stream.setString(delta);
//        stream << auxDelta;
//    }
//    QString retVal;
//    stream.setString(&retVal);
//    stream << ts;
//    stream.flush();
//    return retVal;
//}

//uint64_t ElapsedTimerWrapper::getElapsed() {
//    uint64_t ts = t->elapsed();
//    uint64_t delta = ts - lastTimestamp;
//    lastTimestamp = ts;
//    return delta;
//}

//QString ElapsedTimerWrapper::getElapsedStr() {
//    QString retVal;
//    QTextStream stream(&retVal);
//    stream << getElapsed();
//    stream.flush();
//    return retVal;
//}


