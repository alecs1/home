#ifndef GLOBAL_H
#define GLOBAL_H

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
