#ifndef GLOBAL_H
#define GLOBAL_H

#include <stdint.h>

#ifdef WIN32
#define __func__ __FUNCTION__
#define __PRETTY_FUNCTION__  __FUNCTION__
#endif

#if defined(Q_OS_ANDROID)
#include <log.h>
#define printf(...) __android_log_print(ANDROID_LOG_DEBUG, "FreeGo", __VA_ARGS__);
#endif

#define FREEGO_PASS_MOVE (-2)
#define PASS_COUNT_TO_FINISH (2)

enum class GameState:uint8_t  {
    Initial = 0, //At GUI start-up, clicking on the board will start the game
    Resumed, //TODO - how is Resumed diferent from Started?
    Stopped,
    Started
};

enum class PlayerType:uint8_t {
    AI = 0,
    LocalHuman,
    Network,
    None
};


enum class PlatformType:uint8_t {
    LinuxDesktop = 0,
    Android,
    WindowsDesktop
};

PlatformType platformType();

#endif // GLOBAL_H
