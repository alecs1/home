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

unsigned int countStones(GameStruct* game) {
    unsigned int count = 0;
    for(int i = 0; i < game->size; i++) {
        for(int j = 0; j < game->size; j++)
            if (game->state[i][j] > 0)
                count += 1;
    }
    return count;
}

int otherColour(int crtColour) {
    if (crtColour == WHITE)
        return BLACK;
    else if (crtColour == BLACK)
        return WHITE;
    else
        return -1;
}

QString colourName(int colour) {
    if (colour == EMPTY)
        return "Empty";
    else if (colour == WHITE)
        return "White";
    else if (colour == BLACK)
        return "Black";
}
