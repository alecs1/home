#include "AIThread.h"

extern "C" {
#include "engine/gnugo.h"
}

#include <QMutex>

#include "GoTable.h"
#include "Logger.h"


AIThread::AIThread(QMutex *mutex) : mutex(mutex) {

}

bool AIThread::run_genmove(board_lib_state_struct *internal_state, int color, int AIStrength) {
    mutex->lock();
    Logger::log(QString("%1 - color=%2").arg(__func__).arg(color));
    if(running)
        return false;

    running = true;
    p.operation = OpType::do_genmove;
    p.internal_state = internal_state;
    p.color = color;
    p.strength = AIStrength;
    p.value = 0;
    p.resign = 0;
    p.result = 0;
    start();
    return true;
}

bool AIThread::run_gnugo_estimate_score() {
    mutex->lock();
    if(running)
        return false;
    running = true;
    p.operation = OpType::gnugo_estimate_score;
    start();
    return true;
}

void AIThread::run_value_moves(board_lib_state_struct* internal_state, int colour) {
//    if (mutex->tryLock() == false) {
//        printf("%s - avoided crash with mutex, but there's a logical error\n", __func__);
//        mutex->lock();
//    }

    //value_moves(internal_state, colour, 0.0, 0.0, 1);
    //run genmove to fill in best_move_values
    set_level(2);
    int val = genmove(internal_state, colour, NULL, NULL);
    Q_UNUSED(val);
    mutex->unlock();;
}

void AIThread::run() {
    printf("%s - running on thread %p\n", __func__, QThread::currentThreadId());

//    if (mutex->tryLock() == false) {
//        printf("%s - avoided crash with mutex, but there's a logical error\n", __func__);
//        mutex->lock();
//    }
    set_level(p.strength);
    p.result = genmove(p.internal_state, p.color, &p.move_value, &p.resign);

    int move = p.result;
    if (p.resign) {
        Logger::log(QString("%1 - AI has decided to resign, will compute finals scores.").arg(__func__));
        float score = gnugo_estimate_score(p.internal_state, NULL, NULL);
        if (score > 0) {
            printf("%s - estimates: white winning by %f\n", __func__, score);
        }
        else {
            printf("%s - estimates: black winning by %f\n", __func__, -score);
        }
        //emit AIQuitsGame(true);
    }
    else {
        QPoint point = GoTable::fromGnuGoPos(move);
        printf("%s - AI has finished, move of value=%f, at %d, %d\n", __func__, p.move_value, point.y(), point.x());
        //emit AIThreadPlaceStone(point.y(), point.x());
    }

    running = false;
    mutex->unlock();
}
