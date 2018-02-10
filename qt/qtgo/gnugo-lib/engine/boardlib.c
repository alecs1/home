/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,   *
 * 2008, 2009, 2010 and 2011 by the Free Software Foundation.        *
 *                                                                   *
 * This program is free software; you can redistribute it and/or     *
 * modify it under the terms of the GNU General Public License as    *
 * published by the Free Software Foundation - version 3 or          *
 * (at your option) any later version.                               *
 *                                                                   *
 * This program is distributed in the hope that it will be useful,   *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of    *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     *
 * GNU General Public License in file COPYING for more details.      *
 *                                                                   *
 * You should have received a copy of the GNU General Public         *
 * License along with this program; if not, write to the Free        *
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor,       *
 * Boston, MA 02111, USA.                                            *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


/* This file contains the global functions of the board library libboard.a. */

#include "board.h"
#include "hash.h"

/* The board state itself. */
void init_board_lib_state_struct(board_lib_state_struct* board_lib_state) {
    board_lib_state->board_size = DEFAULT_BOARD_SIZE; /* board size */
    //board_lib_state->board[BOARDSIZE];
    //board_lib_state->board_ko_pos;
    //board_lib_state->white_captured;    /* number of black and white stones captured */
    //board_lib_state->black_captured;

//    bard_lib_state->initial_board[BOARDSIZE];
//    board_lib_state->initial_board_ko_pos;
//    board_lib_state->initial_white_captured;
//    board_lib_state->initial_black_captured;
//    board_lib_state->move_history_color[MAX_MOVE_HISTORY];
//    board_lib_state->move_history_pos[MAX_MOVE_HISTORY];
//    board_lib_state->move_history_hash[MAX_MOVE_HISTORY];
//    board_lib_state->move_history_pointer;

    board_lib_state->komi = 0.0;
    board_lib_state->handicap = 0;
//    board_lib_state->movenum;
    board_lib_state->suicide_rule = FORBIDDEN;
    board_lib_state->ko_rule = SIMPLE;


    //signed char shadow[BOARDMAX];

    /* Hashing of positions. */
    //Hash_data board_hash;

    board_lib_state->stackp = 0;             /* stack pointer */
    //int position_number;    /* position number */

    /* Some statistics gathered partly in board.c and hash.c */
    //struct stats_data stats;

    /* Variation tracking in SGF trees: */
    board_lib_state->count_variations  = 0;
    board_lib_state->sgf_dumptree = NULL;
    board_lib_state->position_number = 0;
}
