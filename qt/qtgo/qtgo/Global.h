#ifndef GLOBAL_H
#define GLOBAL_H

enum class GameState:uint8_t  {
    Initial, //At GUI start-up, clicking on the board will start the game
    Stopped,
    Started
};

enum class GameType:uint8_t {
    AI,
    LocalHuman,
    Network
};

struct SGameSettings {
    int size = 19;
    int AILevel = 1;
    //TODO - some game type (vs computer or human player)
};

#endif // GLOBAL_H
