#include "GameStruct.h"

//TODO - nothing here yet.
bool GamePlaceStone(GameStruct* game, int row, int col, int player) {
    if (game->state[row][col] == 0) {
        game->state[row][col] = player;
        return true;
    }
    return false;
}

bool GameCanPlaceStone(GameStruct* game, int row, int col, int player) {
    if (game->state[row][col] == 0)
        return true;
    return false;
}
