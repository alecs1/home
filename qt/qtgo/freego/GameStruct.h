#pragma once

#include <QString>

//use the same definitions as GnuGo. If board.h is not included, define here
#ifndef _BOARD_H_
enum colors {
  EMPTY,
  WHITE,
  BLACK,
  GRAY,
  GRAY_WHITE,
  GRAY_BLACK,
  WEAK_KO,
  NUM_KOMASTER_STATES
};
#endif

//what gnugo is doing may actually be useful, table looks like
/*
xxxx
x000
x000
x000
*/
struct GameStruct {
    int size;
    int state[19][19];
};

bool GamePlaceStone(GameStruct* game, int row, int col, int player);
bool GameCanPlaceStone(GameStruct* game, int row, int col, int player);
unsigned int countStones(GameStruct* game);


int otherColour(int crtColour);
QString colourName(int colour);
