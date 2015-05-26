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

#define FREEGO_PASS_MOVE (-2)
#define FREEGO_VERSION "0.84"
#define PASS_COUNT_TO_FINISH (4)

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
    struct Handicap {
        float komi = 6.5;
        int handicap = 0;
        bool handicapPlacementFree = false;
    };
    Handicap handicap;

    //information used for saving
    bool estimateScore;

    //game state, move to another struct if there are more elements
    int crtPlayer;
};

#include <QString>
struct SProgramSettings {
    static const int maxSoundsVolume = 1; //hack, we only accept 0 or 1 for now
    uint32_t soundsVolume = 100;
    QString tableColour;
};

enum class PlatformType:uint8_t {
    LinuxDesktop = 0,
    Android,
    WindowsDesktop
};

PlatformType platformType();

//Extra information about a game, don't know if this is the right place
struct SAuxGameInfo {
    QString comment;
    QString freeGoVersion;
    QString gameDate;
};

#endif // GLOBAL_H
