#ifndef GLOBAL_H
#define GLOBAL_H

#if defined(Q_OS_ANDROID)
#include <log.h>
#define printf(...) __android_log_print(ANDROID_LOG_DEBUG, "FreeGo", __VA_ARGS__);
#endif

enum class GameState:uint8_t  {
    Initial, //At GUI start-up, clicking on the board will start the game
    Stopped,
    Started
};

enum class PlayerType:uint8_t {
    AI = 0,
    LocalHuman,
    Network
};

struct SGameSettings {
    int size = 19;
    int AILevel = 1;
    PlayerType black = PlayerType::LocalHuman;
    PlayerType white = PlayerType::AI;
};

#endif // GLOBAL_H
