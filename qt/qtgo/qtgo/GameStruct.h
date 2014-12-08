#ifndef GAMESTRUCT_H
#define GAMESTRUCT_H

//0 - no player (for positions on table)
//1 - black player
//2 - white player

struct GameStruct {
    int size;
    int state[19][19];
};

bool GamePlaceStone(GameStruct* game, int row, int col, int player);
bool GameCanPlaceStone(GameStruct* game, int row, int col, int player);

#endif // GAMESTRUCT_H
