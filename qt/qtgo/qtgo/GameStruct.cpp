#include "GameStruct.h"

//TODO - nothing here yet.
bool GamePlaceStone(GameStruct* game, int row, int col, int player) {
    if (game->state[row][col] == 0) {
        game->state[row][col] = player;
        return true;
    }
    return false;
}

//TODO - replace with is_legal
bool GameCanPlaceStone(GameStruct* game, int row, int col, int player) {
    if (game->state[row][col] == 0)
        return true;
    return false;
}

int otherColour(int crtColour) {
    if (crtColour == WHITE)
        return BLACK;
    else if (crtColour == BLACK)
        return WHITE;
    else
        return -1;
}
