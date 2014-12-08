#ifndef GAMESTRUCT_H
#define GAMESTRUCT_H


struct GameStruct {
    int size;
    int state[19][19]; //actually state[size][size];
};

int placeStone(GameStruct* game, int row, int col);

#endif // GAMESTRUCT_H
