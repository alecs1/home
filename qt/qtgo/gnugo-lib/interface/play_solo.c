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

#include "gnugo.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "interface.h"

#include "liberty.h" /* to get to the stats */

#include "sgftree.h"
#include "random.h"
#include "gg_utils.h"

void
play_solo(Gameinfo *gameinfo, int moves)
{
  SGFTree sgftree;
  int passes = 0; /* num. consecutive passes */
  float move_value;
  double t1, t2;
  int save_moves = moves;

  struct stats_data totalstats;
  int total_owl_count = 0;

  /* It tends not to be very imaginative in the opening,
   * so we scatter a few stones randomly to start with.
   * We add two random numbers to reduce the probability
   * of playing stones near the edge.
   */
  
  int n = 6 + 2*gg_rand()%5;
  int i, j;

  board_lib_state_struct* internal_state = malloc(sizeof(board_lib_state_struct));
  internal_state->komi = 5.5;

  sgftree_clear(&sgftree);
  sgftreeCreateHeaderNode(&sgftree, internal_state->board_size, internal_state->komi, internal_state->handicap);
  sgf_write_header(sgftree.root, 1, get_random_seed(), 5.5, internal_state->handicap,
                   get_level(), chinese_rules);
 
  /* Generate some random moves. */
  if (internal_state->board_size > 6) {
    do {
      do {
	i = (gg_rand() % 4) + (gg_rand() % (internal_state->board_size - 4));
	j = (gg_rand() % 4) + (gg_rand() % (internal_state->board_size - 4));
      } while (!is_allowed_move(internal_state, POS(i, j), gameinfo->to_move));

      gnugo_play_move(internal_state, POS(i, j), gameinfo->to_move);
      sgftreeAddPlay(&sgftree, gameinfo->to_move, i, j);
      sgftreeAddComment(&sgftree, "random move");
      gameinfo->to_move = OTHER_COLOR(gameinfo->to_move);
    } while (--n > 0);
  }
  
  t1 = gg_cputime();
  memset(&totalstats, '\0', sizeof(totalstats));
  while (passes < 2 && --moves >= 0) {
    int move;
    reset_owl_node_counter();
    move = genmove(internal_state, gameinfo->to_move, &move_value, NULL);

    gnugo_play_move(internal_state, move, gameinfo->to_move);
    sgffile_add_debuginfo(internal_state, sgftree.lastnode, move_value);
    sgftreeAddPlay(&sgftree, gameinfo->to_move, I(move), J(move));
    sgffile_output(&sgftree);
    gameinfo->to_move = OTHER_COLOR(gameinfo->to_move);

    if (move == PASS_MOVE) {
      passes++;
      printf("%s(%d): Pass\n", gameinfo->to_move == BLACK ? "Black" : "White",
         internal_state->movenum);
    }
    else {
      passes = 0;
      gprintf(internal_state, "%s(%d): %1m\n", gameinfo->to_move == BLACK ? "Black" : "White",
          internal_state->movenum, move);
    }

    totalstats.nodes                    += internal_state->stats.nodes;
    totalstats.read_result_entered      += internal_state->stats.read_result_entered;
    totalstats.read_result_hits         += internal_state->stats.read_result_hits;
    totalstats.trusted_read_result_hits += internal_state->stats.trusted_read_result_hits;
    total_owl_count                     += get_owl_node_counter();
  }
  t2 = gg_cputime();
  
  /* Two passes and it's over. (EMPTY == BOTH) */
  who_wins(internal_state, EMPTY, stdout);

  {
    float score = gnugo_estimate_score(internal_state, NULL, NULL);
    sgfWriteResult(sgftree.root, score, 1);
  }
  sgffile_output(&sgftree);

  printf("%10d moves played in %0.3f seconds\n", save_moves-moves, t2-t1);
  if (save_moves != moves)
    printf("%10.3f seconds/move\n", (t2-t1)/(save_moves-moves));
  
  printf("%10d nodes\n", totalstats.nodes);
  printf("%10d read results entered\n", totalstats.read_result_entered);
  printf("%10d read result hits\n", totalstats.read_result_hits);
  printf("%10d trusted read result hits\n",
	 totalstats.trusted_read_result_hits);
  printf("%10d owl nodes\n", total_owl_count);
}


/* ================================================================ */


/*
 * Load SGF file and run genmove().
 */

void 
load_and_analyze_sgf_file(board_lib_state_struct* internal_state,
                          Gameinfo *gameinfo)
{
  SGFTree sgftree;
  int move;
  int next;
  float move_value;
  
  next = gameinfo->to_move;
  sgftree = gameinfo->game_record;

  if (metamachine)
    sgffile_begindump(internal_state, &sgftree);

  move = genmove(internal_state, next, &move_value, NULL);

  gprintf(internal_state, "%s move %1m\n", next == WHITE ? "white (O)" : "black (X)", move);

  if (metamachine)
    sgffile_enddump(internal_state, outfilename);
  else {
    gnugo_play_move(internal_state, move, next);
    sgftreeAddPlay(&sgftree, next, I(move), J(move));
    sgftreeAddComment(&sgftree, "load and analyze mode");
    sgffile_add_debuginfo(internal_state, sgftree.lastnode, move_value);
    sgffile_output(&sgftree);
  }
}


/*
 * Load SGF file and score the game
 * scoringmode:
 * estimate  - estimate territorial balance
 * finish    - finish the game by selfplaying and then count the score quickly
 * aftermath - like 'finish' but also play out the aftermath in order to
 *             get an accurate score
 */

#define ESTIMATE  0
#define FINISH    1
#define AFTERMATH 2

void 
load_and_score_sgf_file(board_lib_state_struct* internal_state,
                        SGFTree *tree, Gameinfo *gameinfo,
			const char *scoringmode)
{
  int move;
  float move_value;
  char *tempc = NULL;
  char text[250];
  char winner;
  int next;
  int pass = 0;
  int method;
  float score;
  SGFTree local_tree;
  SGFTree *score_tree = tree;
  
  /* Default scoring method is ESTIMATE since it's fastest. */
  method = ESTIMATE;
  if (strcmp(scoringmode, "finish") == 0)
    method = FINISH;
  else if (strcmp(scoringmode, "aftermath") == 0)
    method = AFTERMATH;

  /* For aftermath scoring we compress the previous moves to a static
   * board position in the output sgf. This helps a lot when debugging
   * scoring mistakes. We don't do this for the finish method,
   * however, since users may be better served by having GNU Go's
   * selfplay added to the original game record.
   */
  if (method == AFTERMATH) {
    sgftree_clear(&local_tree);
    /* Modify komi to compensate for captured stones. We start at a
     * setup position and since there is no standard sgf property to
     * tell the number of captured stones, a modified komi is the best
     * available solution.
     */
    sgftreeCreateHeaderNode(&local_tree, internal_state->board_size,
                internal_state->komi + internal_state->black_captured - internal_state->white_captured, internal_state->handicap);
    sgffile_printboard(internal_state, &local_tree);
    sgfAddProperty(local_tree.lastnode, "PL",
		   gameinfo->to_move == WHITE ? "W" : "B");
    score_tree = &local_tree;
  }
  
  next = gameinfo->to_move;
  reset_engine(internal_state);
  
  /* Complete the game by selfplay for the finish and aftermath methods. */
  if (method != ESTIMATE) {
    doing_scoring = 1;
    while (pass < 2) {
      move = genmove_conservative(internal_state, next, &move_value);
      if (move != PASS_MOVE) {
	pass = 0;
	gprintf(internal_state, "%d %s move %1m\n", internal_state->movenum,
		next == WHITE ? "white (O)" : "black (X)", move);
      }
      else {
	pass++;
	gprintf(internal_state, "%d %s move PASS\n", internal_state->movenum, 
		next == WHITE ? "white (O)" : "black (X)");
      }
      play_move(internal_state, move, next);
      sgffile_add_debuginfo(internal_state, score_tree->lastnode, move_value);
      sgftreeAddPlay(score_tree, next, I(move), J(move));
      sgffile_output(score_tree);
      next = OTHER_COLOR(next);
    }
    doing_scoring = 0;
  }
  
  /* Calculate the score. */
  if (method == AFTERMATH)
    score = aftermath_compute_score(internal_state, next, score_tree);
  else
    score = gnugo_estimate_score(internal_state, NULL, NULL);
  
  if (score < 0.0) {
    sprintf(text, "Black wins by %1.1f points\n", -score);
    winner = 'B';
  }
  else if (score > 0.0) {
    sprintf(text, "White wins by %1.1f points\n", score);
    winner = 'W';
  }
  else {
    sprintf(text, "Jigo\n");
    winner = '0';
  }
  fputs(text, stdout);
  sgftreeAddComment(score_tree, text);

  /* For the finish and aftermath methods we compare the score with
   * what's stored in the game record.
   *
   * FIXME: No comparison is made if the stored result was 0. Wins by
   *        time or forfeit are not handled either.
   *
   * FIXME: Does anybody actually care about this information? Just
   *        removing this piece of code is a tempting alternative.
   */
  if (method != ESTIMATE && sgfGetCharProperty(tree->root, "RE", &tempc)) {
    char dummy;
    float result;
    if (sscanf(tempc, "%1c%f", &dummy, &result) == 2) {
      fprintf(stdout, "Result from file: %c+%1.1f\n", dummy, result);
      fputs("GNU Go result and result from file are ", stdout);
      if (result == fabs(score) && winner == dummy)
	fputs("identical\n", stdout);
      else
	fputs("different\n", stdout);
      
    }
    else {
      if (tempc[2] == 'R') {
	fprintf(stdout, "Result from file: Resign\n");
	fputs("GNU Go result and result from file are ", stdout);
	if (tempc[0] == winner)
	  fputs("identical\n", stdout);
	else
	  fputs("different\n", stdout);
      }
    }
  }

  if (method != ESTIMATE)
    sgfWriteResult(score_tree->root, score, 1);
  
  sgffile_output(score_tree);
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
