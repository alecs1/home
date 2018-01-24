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

#ifndef _BOARD_H_
#define _BOARD_H_

#include <stdarg.h>
#include "config.h"
#include "sgftree.h"
#include "winsocket.h"
#include "structures.h"

/* FIXME: This is very ugly but we can't include hash.h until we have
 * defined Intersection. And we do need to include it before using
 * Hash_data.
 */
#include "hash.h"

/* Colors and komaster states. */
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

#define COLOR_NAMES \
  "empty", \
  "white", \
  "black", \
  "gray", \
  "gray_white", \
  "gray_black", \
  "weak_ko"

const char *color_to_string(struct board_lib_state_struct *internal_state, int color);

/* ================================================================ */
/*                         global variables                         */
/* ================================================================ */




/* This struct holds the internal board state. */
struct board_state {
  int board_size;

  Intersection board[BOARDSIZE];
  int board_ko_pos;
  int black_captured;
  int white_captured;

  Intersection initial_board[BOARDSIZE];
  int initial_board_ko_pos;
  int initial_white_captured;
  int initial_black_captured;
  int move_history_color[MAX_MOVE_HISTORY];
  int move_history_pos[MAX_MOVE_HISTORY];
  Hash_data move_history_hash[MAX_MOVE_HISTORY];
  int move_history_pointer;

  float komi;
  int handicap;
  int move_number;
};


/* ================================================================ */
/*                        board.c functions                         */
/* ================================================================ */


/* Functions handling the permanent board state. */
void init_board_lib_state_struct(board_lib_state_struct *internal_state);
void clear_board(board_lib_state_struct *internal_state);
int test_gray_border(board_lib_state_struct *internal_state);
void setup_board(Intersection new_board[MAX_BOARD][MAX_BOARD], int ko_pos,
                 int *last, float new_komi, int w_captured, int b_captured);
void add_stone(struct board_lib_state_struct *internal_state, int pos, int color);
void remove_stone(board_lib_state_struct *internal_state, int pos);
void play_move(struct board_lib_state_struct *internal_state, int pos, int color);
int undo_move(struct board_lib_state_struct *internal_state, int n);

void store_board(struct board_lib_state_struct *internal_state, struct board_state *state);
void restore_board(struct board_lib_state_struct *internal_state, struct board_state *state);

/* Information about the permanent board. */
int get_last_move(struct board_lib_state_struct *internal_state);
int get_last_player(struct board_lib_state_struct *internal_state);
int get_last_opponent_move(struct board_lib_state_struct *internal_state, int color);
int stones_on_board(struct board_lib_state_struct *internal_state, int color);

/* Functions handling the variable board state. */
int trymove(struct board_lib_state_struct *internal_state, int pos, int color, const char *message, int str);
int tryko(struct board_lib_state_struct *internal_state, int pos, int color, const char *message);
void popgo(struct board_lib_state_struct *internal_state);
int komaster_trymove(struct board_lib_state_struct *internal_state, int pos, int color,
                     const char *message, int str,
                     int *is_conditional_ko, int consider_conditional_ko);
int get_komaster(void);
int get_kom_pos(void);

int move_in_stack(struct board_lib_state_struct *internal_state, int pos, int cutoff);
void get_move_from_stack(struct board_lib_state_struct *internal_state, int k, int *move, int *color);
void dump_stack(struct board_lib_state_struct *internal_state);
void do_dump_stack(struct board_lib_state_struct *internal_state);

void reset_trymove_counter(void);
int get_trymove_counter(void);

/* move properties */
int is_pass(int pos);
int is_legal(board_lib_state_struct *internal_state,
             int pos, int color);
int is_suicide(struct board_lib_state_struct *internal_state, int pos, int color);
int is_illegal_ko_capture(struct board_lib_state_struct *internal_state, int pos, int color);
int is_allowed_move(struct board_lib_state_struct *internal_state, int pos, int color);
int is_ko(board_lib_state_struct *internal_state,
          int pos, int color, int *ko_pos);
int is_ko_point(board_lib_state_struct *internal_state,
                int pos);
int does_capture_something(struct board_lib_state_struct *internal_state,
                           int pos, int color);
int is_self_atari(struct board_lib_state_struct *internal_state, int pos, int color);

/* Purely geometric functions. */
int is_edge_vertex(board_lib_state_struct *internal_state,
                   int pos);
int is_corner_vertex(board_lib_state_struct *internal_state,
                     int pos);
int edge_distance(struct board_lib_state_struct *internal_state, int pos);
int square_dist(int pos1, int pos2);
int rotate1(struct board_lib_state_struct *internal_state, int pos, int rot);

/* Basic string information. */
int find_origin(struct board_lib_state_struct *internal_state, int str);
int chainlinks(struct board_lib_state_struct *internal_state, int str, int adj[MAXCHAIN]);
int chainlinks2(struct board_lib_state_struct *internal_state, int str, int adj[MAXCHAIN], int lib);
int chainlinks3(struct board_lib_state_struct *internal_state, int str, int adj[MAXCHAIN], int lib);
int extended_chainlinks(struct board_lib_state_struct *internal_state,
                        int str, int adj[MAXCHAIN], int both_colors);

int liberty_of_string(struct board_lib_state_struct *internal_state, int pos, int str);
int second_order_liberty_of_string(struct board_lib_state_struct *internal_state, int pos, int str);
int neighbor_of_string(struct board_lib_state_struct *internal_state, int pos, int str);
int has_neighbor(board_lib_state_struct *internal_state,
                 int pos, int color);
int same_string(board_lib_state_struct *internal_state,
                int str1, int str2);
int adjacent_strings(struct board_lib_state_struct *internal_state, int str1, int str2);
void mark_string(struct board_lib_state_struct *internal_state, int str, signed char mx[BOARDMAX], signed char mark);
int are_neighbors(struct board_lib_state_struct *internal_state, int pos1, int pos2);

/* Count and/or find liberties at (pos). */
int countlib(board_lib_state_struct *internal_state,
             int str);
int findlib(board_lib_state_struct *internal_state,
            int str, int maxlib, int *libs);
int fastlib(struct board_lib_state_struct *internal_state, int pos, int color, int ignore_captures);
int approxlib(struct board_lib_state_struct *internal_state, int pos, int color, int maxlib, int *libs);
int accuratelib(struct board_lib_state_struct *internal_state, int pos, int color, int maxlib, int *libs);
int count_common_libs(struct board_lib_state_struct *internal_state, int str1, int str2);
int find_common_libs(struct board_lib_state_struct *internal_state, int str1, int str2, int maxlib, int *libs);
int have_common_lib(struct board_lib_state_struct *internal_state, int str1, int str2, int *lib);

/* Count the number of stones in a string. */
int countstones(struct board_lib_state_struct *internal_state, int str);
int findstones(struct board_lib_state_struct *internal_state, int str, int maxstones, int *stones);
int count_adjacent_stones(struct board_lib_state_struct *internal_state, int str1, int str2, int maxstones);

/* Detect a special shape. */
int send_two_return_one(struct board_lib_state_struct *internal_state, int move, int color);

/* Special function for reading.c */
void incremental_order_moves(struct board_lib_state_struct *internal_state,
                             int move, int color, int string,
                             int *number_edges, int *number_same_string,
                             int *number_own, int *number_opponent,
                             int *captured_stones, int *threatened_stones,
                             int *saved_stones, int *number_open);

/* Board caches initialization functions. */
void clear_approxlib_cache(void);
void clear_accuratelib_cache(void);
  

/* Is this point inside the board? */
#if 0
#define ON_BOARD2(i, j) ((i)>=0 && (j)>=0 && (i)<board_size && (j)<board_size)
#else
/*
 * For the case when expr can only be slightly negative,
 *    if (expr < 0 || expr > something)
 * is equivalent to
 *    if ((unsigned) expr > something)
 *
 * (I think gcc knows this trick, but it does no harm to
 *  encode it explicitly since it saves typing !)
 */
#define ON_BOARD2(internal_state, i, j) ((unsigned) (i) < (unsigned) internal_state->board_size &&\
                         (unsigned) (j) < (unsigned) internal_state->board_size)
#endif

#define ASSERT_ON_BOARD2(internal_state, i, j) ASSERT2(internal_state, ON_BOARD2(internal_state, (i), (j)), (i), (j))

#define ON_BOARD1(internal_state, pos) (((unsigned) (pos) < BOARDSIZE) && internal_state->board[pos] != GRAY)
#define ON_BOARD(internal_state, pos) (internal_state->board[pos] != GRAY)
#define ASSERT_ON_BOARD1(internal_state, pos) ASSERT1(internal_state, ON_BOARD1(internal_state, pos), (pos))

/* Coordinates for the eight directions, ordered
 * south, west, north, east, southwest, northwest, northeast, southeast.
 * Defined in board.c.
 */
extern int deltai[8]; /* = { 1,  0, -1,  0,  1, -1, -1, 1}; */
extern int deltaj[8]; /* = { 0, -1,  0,  1, -1, -1,  1, 1}; */
extern int delta[8];  /* = { NS, -1, -NS, 1, NS-1, -NS-1, -NS+1, NS+1}; */



/* ================================================================ */
/*                          Other functions                         */
/* ================================================================ */


/* SGF routines for debugging purposes in sgffile.c */
void sgffile_begindump(board_lib_state_struct *internal_state,
                       struct SGFTree_t *tree);
void sgffile_enddump(board_lib_state_struct *internal_state,
                     const char *filename);





/* printutils.c */
int gprintf(struct board_lib_state_struct *internal_state, const char *fmt, ...);
void vgprintf(struct board_lib_state_struct *internal_state, FILE *outputfile, const char *fmt, va_list ap);
void mprintf(struct board_lib_state_struct *internal_state, const char *fmt, ...);
void gfprintf(board_lib_state_struct *internal_state, FILE *outfile, const char *fmt, ...);

const char *color_to_string(struct board_lib_state_struct *internal_state,
                            int color);
const char *location_to_string(struct board_lib_state_struct *internal_state,
                               int pos);
void location_to_buffer(struct board_lib_state_struct *internal_state,
                        int pos, char *buf);

int string_to_location(int boardsize, const char *str);

int is_hoshi_point(struct board_lib_state_struct *internal_state, int m, int n);
void draw_letter_coordinates(struct board_lib_state_struct *internal_state, FILE *outfile);
void simple_showboard(struct board_lib_state_struct *internal_state, FILE *outfile);

void mark_goal_in_sgf(struct board_lib_state_struct *internal_state, signed char goal[BOARDMAX]);

/* ================================================================ */
/*                         assertions                               */
/* ================================================================ */

/* Our own abort() which prints board state on the way out.
 * (pos) is a "relevant" board position for info.
 */
void abortgo(struct board_lib_state_struct *internal_state, const char *file, int line, const char *msg, int pos)
#ifdef __GNUC__
	__attribute__ ((noreturn))
#endif
	;

#ifdef GG_TURN_OFF_ASSERTS
#define ASSERT2(x, i, j)
#define ASSERT1(x, pos)
#else
/* avoid dangling else */
/* FIXME: Should probably re-write these using do {...} while (0) idiom. */
#define ASSERT2(internal_state, x, i, j) if (x) ; else abortgo(internal_state, __FILE__, __LINE__, #x, POS(i, j))
#define ASSERT1(internal_state, x, pos) if (x) ; else abortgo(internal_state, __FILE__, __LINE__, #x, pos)
#endif

#define gg_assert(internal_state, x) ASSERT1(internal_state, x, NO_MOVE)

/* Are we using valgrind memory checking? */
#if USE_VALGRIND
#include <valgrind/memcheck.h>
#else
#define VALGRIND_MAKE_WRITABLE(a, b)
#endif

#endif  /* _BOARD_H_ */


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
