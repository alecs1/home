#pragma once

#include <QThread>
class QMutex;

struct board_lib_state_struct;

class AIThread : public QThread {
    void run() override;

public:
    AIThread(QMutex* mutex);
    bool run_genmove(board_lib_state_struct* internal_state, int color, int AIStrength);
    bool run_gnugo_estimate_score();
    void run_value_moves(board_lib_state_struct* internal_state, int colour);

public:
    QMutex* mutex = nullptr;

signals:
    //void AIThreadPlaceStone(int row, int col);
    //void AIQuitsGame(bool accurate);

private:
    bool running = false;
    enum class OpType:uint8_t {
        //keep the operation names in sync with the corresponding functions
        do_genmove = 1,
        gnugo_estimate_score
    };

    struct Parameters {
        OpType operation;

        //params for do_genmove
        board_lib_state_struct* internal_state = nullptr;
        int strength;
        int color;
        float move_value;
        float value;
        int resign;
        int result;
    };
    Parameters p;
};
