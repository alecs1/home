#ifndef GLOBAL_H
#define GLOBAL_H

#include <stdint.h>

#ifdef WIN32
#define __func__ __FUNCTION__
#endif

#if defined(Q_OS_ANDROID)
#include <log.h>
#define printf(...) __android_log_print(ANDROID_LOG_DEBUG, "FreeGo", __VA_ARGS__);
#endif

enum class GameState:uint8_t  {
    Initial = 0, //At GUI start-up, clicking on the board will start the game
    Resumed,
    Stopped,
    Started
};

enum class PlayerType:uint8_t {
    AI = 0,
    LocalHuman,
    Network,
    None
};

struct SGameSettings {
    int size = 19;
    int blackAIStrength = 0;
    int whiteAIStrength = 0;
    PlayerType black = PlayerType::LocalHuman;
    PlayerType white = PlayerType::AI;
    //information used for saving
    bool estimateScore;

    //game state, move to another struct if there are more elements
    int crtPlayer;
};

enum class PlatformType:uint8_t {
    LinuxDesktop = 0,
    Android,
    WindowsDesktop
};

PlatformType platformType();

//Extra information about a game, don't know if this is the right place
#include <QString>
struct SAuxGameInfo {
    QString comment;
    QString freeGoVersion;
    QString gameDate;
};

#endif // GLOBAL_H
