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

/* ================================================================ */
/*      Show status for a string, a dragon, etc in an SGF file.     */
/* ================================================================ */

#include "gnugo.h"

#include <stdio.h>
#include <string.h>

#include "liberty.h"
#include "sgftree.h"


/* 
 * decide_string tries to attack and defend the string at (pos),
 * and then writes the number of variations considered in the attack
 * and defence to the sgf file.
 */

void
decide_string(board_lib_state_struct *internal_state,
              int pos)
{
  int aa, dd;
  int acode, dcode;
  SGFTree tree;
  
  if (internal_state->board[pos] == EMPTY) {
    fprintf(stderr, "gnugo: --decide-string called on an empty vertex\n");
    return;
  }

  if (*outfilename)
    sgffile_begindump(internal_state, &tree);

  /* Prepare pattern matcher and reading code. */
  reset_engine(internal_state);

  internal_state->count_variations = 1;
  acode = attack(internal_state, pos, &aa);
  if (acode) {
    if (acode == WIN)
      gprintf(internal_state, "%1m can be attacked at %1m (%d variations)\n", 
	      pos, aa, internal_state->count_variations);
    else if (acode == KO_A)
	gprintf(internal_state, "%1m can be attacked with ko (good) at %1m (%d variations)\n", 
	      pos, aa, internal_state->count_variations);
    else if (acode == KO_B)
	gprintf(internal_state, "%1m can be attacked with ko (bad) at %1m (%d variations)\n", 
		pos, aa, internal_state->count_variations);

    if (debug & DEBUG_READING_PERFORMANCE) {
      gprintf(internal_state, "Reading shadow: \n");
      draw_reading_shadow(internal_state);
    }

    internal_state->count_variations = 1;
    dcode = find_defense(internal_state, pos, &dd);
    if (dcode) {
      if (dcode == WIN)
	gprintf(internal_state, "%1m can be defended at %1m (%d variations)\n", 
		pos, dd, internal_state->count_variations);
      else if (dcode == KO_A)
	gprintf(internal_state, "%1m can be defended with ko (good) at %1m (%d variations)\n", 
		pos, dd, internal_state->count_variations);
      else if (dcode == KO_B)
	gprintf(internal_state, "%1m can be defended with ko (bad) at %1m (%d variations)\n", 
		pos, dd, internal_state->count_variations);
    }
    else
      gprintf(internal_state, "%1m cannot be defended (%d variations)\n", 
	      pos, internal_state->count_variations);
    if (debug & DEBUG_READING_PERFORMANCE) {
      gprintf(internal_state, "Reading shadow: \n");
      draw_reading_shadow(internal_state);
    }

  }
  else {
    gprintf(internal_state, "%1m cannot be attacked (%d variations)\n", 
	    pos, internal_state->count_variations);
    if (debug & DEBUG_READING_PERFORMANCE) {
      gprintf(internal_state, "Reading shadow: \n");
      draw_reading_shadow(internal_state);
    }
  }

  sgffile_enddump(internal_state, outfilename);
  internal_state->count_variations = 0;
}


/* 
 * decide_connection tries to connect and disconnect the strings at
 * (apos) and (bpos), and then writes the number of variations
 * considered in the attack and defence to the sgf file.
 */

void
decide_connection(board_lib_state_struct *internal_state,
                  int apos, int bpos)
{
  int move;
  int result;
  SGFTree tree;

  ASSERT_ON_BOARD1(internal_state, apos);
  ASSERT_ON_BOARD1(internal_state, bpos);
  
  if (internal_state->board[apos] == EMPTY || internal_state->board[bpos] == EMPTY) {
    fprintf(stderr, "gnugo: --decide-connection called on an empty vertex\n");
    return;
  }

  if (internal_state->board[apos] != internal_state->board[bpos]) {
    fprintf(stderr, "gnugo: --decide-connection called for strings of different colors\n");
    return;
  }

  if (*outfilename)
    sgffile_begindump(internal_state, &tree);

  /* Prepare pattern matcher and reading code. */
  reset_engine(internal_state);

  internal_state->count_variations = 1;
  result = string_connect(internal_state, apos, bpos, &move);
  if (result == WIN) {
    if (move == NO_MOVE)
      gprintf(internal_state, "%1m and %1m are connected as it stands (%d variations)\n", 
	      apos, bpos, internal_state->count_variations);
    else
	gprintf(internal_state, "%1m and %1m can be connected at %1m (%d variations)\n", 
		apos, bpos, move, internal_state->count_variations);
  }
  else if (result == KO_A)
    gprintf(internal_state, "%1m and %1m can be connected with ko (good) at %1m (%d variations)\n", 
	    apos, bpos, move, internal_state->count_variations);
  else if (result == KO_B)
    gprintf(internal_state, "%1m and %1m can be connected with ko (bad) at %1m (%d variations)\n", 
	    apos, bpos, move, internal_state->count_variations);
  else
    gprintf(internal_state, "%1m and %1m cannot be connected (%d variations)\n", 
	    apos, bpos, internal_state->count_variations);
  
  internal_state->count_variations = 1;
  result = disconnect(internal_state, apos, bpos, &move);
  if (result == WIN) {
    if (move == NO_MOVE)
      gprintf(internal_state, "%1m and %1m are disconnected as it stands (%d variations)\n", 
	      apos, bpos, internal_state->count_variations);
    else
	gprintf(internal_state, "%1m and %1m can be disconnected at %1m (%d variations)\n", 
		apos, bpos, move, internal_state->count_variations);
  }
  else if (result == KO_A)
    gprintf(internal_state, "%1m and %1m can be disconnected with ko (good) at %1m (%d variations)\n", 
	    apos, bpos, move, internal_state->count_variations);
  else if (result == KO_B)
    gprintf(internal_state, "%1m and %1m can be disconnected with ko (bad) at %1m (%d variations)\n", 
	    apos, bpos, move, internal_state->count_variations);
  else
    gprintf(internal_state, "%1m and %1m cannot be disconnected (%d variations)\n", 
	    apos, bpos, internal_state->count_variations);
  
  sgffile_enddump(internal_state, outfilename);
  internal_state->count_variations = 0;
}


/* 
 * decide_owl (formerly called decide_dragon) tries to attack and defend 
 * the dragon at (pos), and then writes the number of variations considered 
 * in the attack and defence to the sgf file.
 */

void
decide_owl(board_lib_state_struct *internal_state,
           int pos)
{
  int move = NO_MOVE;
  int acode, dcode;
  SGFTree tree;
  int result_certain;
  int kworm;

  if (internal_state->board[pos] == EMPTY) {
    fprintf(stderr, "gnugo: --decide-dragon called on an empty vertex\n");
    return;
  }

  /* Prepare pattern matcher and reading code. */
  reset_engine(internal_state);

  silent_examine_position(internal_state, EXAMINE_DRAGONS_WITHOUT_OWL);
  gprintf(internal_state, "finished examine_position\n");

  if (*outfilename)
    sgffile_begindump(internal_state, &tree);

  internal_state->count_variations = 1;
  acode = owl_attack(internal_state, pos, &move, &result_certain, &kworm);
  if (acode) {
    if (acode == WIN) {
      if (move == NO_MOVE)
	gprintf(internal_state, "%1m is dead as it stands", pos);
      else
	gprintf(internal_state, "%1m can be attacked at %1m (%d variations)", 
		pos, move, internal_state->count_variations);
    }
    else if (acode == KO_A)
      gprintf(internal_state, "%1m can be attacked with ko (good) at %1m (%d variations)", 
	      pos, move, internal_state->count_variations);
    else if (acode == KO_B)
      gprintf(internal_state, "%1m can be attacked with ko (bad) at %1m (%d variations)", 
	      pos, move, internal_state->count_variations);
    else if (acode == GAIN)
      gprintf(internal_state, "%1m can be attacked with gain (captures %1m) at %1m (%d variations)", 
	      pos, kworm, move, internal_state->count_variations);
  }
  else 
    gprintf(internal_state, "%1m cannot be attacked (%d variations)", pos, internal_state->count_variations);
  
  if (result_certain)
    gprintf(internal_state, "\n");
  else
    gprintf(internal_state, " result uncertain\n");

  internal_state->count_variations = 1;
  dcode = owl_defend(internal_state, pos, &move, &result_certain, &kworm);

  if (dcode) {
    if (dcode == WIN) {
      if (move == NO_MOVE)
	gprintf(internal_state, "%1m is alive as it stands", pos);
      else 
	gprintf(internal_state, "%1m can be defended at %1m (%d variations)", 
		pos, move, internal_state->count_variations);
    }
    else if (dcode == KO_A)
      gprintf(internal_state, "%1m can be defended with ko (good) at %1m (%d variations)", 
	      pos, move, internal_state->count_variations);
    else if (dcode == KO_B)
      gprintf(internal_state, "%1m can be defended with ko (bad) at %1m (%d variations)", 
	      pos, move, internal_state->count_variations);
    else if (dcode == LOSS)
      gprintf(internal_state, "%1m can be defended with loss (loses %1m) at %1m (%d variations)", 
	      pos, kworm, move, internal_state->count_variations);
  }
  else
    gprintf(internal_state, "%1m cannot be defended (%d variations)",
	    pos, internal_state->count_variations);

  if (result_certain)
    gprintf(internal_state, "\n");
  else
    gprintf(internal_state, " result uncertain\n");
  
  sgffile_enddump(internal_state, outfilename);
  internal_state->count_variations = 0;
}


/* 
 * decide_dragon_data prints the dragon data at (pos).
 */

void
decide_dragon_data(board_lib_state_struct *internal_state,
                   int pos)
{
  if (internal_state->board[pos] == EMPTY) {
    fprintf(stderr, "gnugo: --decide-dragon-data called on an empty vertex\n");
    return;
  }
  reset_engine(internal_state);
  silent_examine_position(internal_state, FULL_EXAMINE_DRAGONS);

  gprintf(internal_state, "Dragon at %1m:\n", pos);
  report_dragon(internal_state, stderr, pos);
}


/* Print the result of the semeai code on the semeai at apos/bpos,
 * optionally writing an sgf file.
 */

void
decide_semeai(board_lib_state_struct *internal_state,
              int apos, int bpos)
{
  SGFTree tree;
  int resulta, resultb, move, result_certain;
  int color = internal_state->board[apos];

  if (color == EMPTY || internal_state->board[bpos] != OTHER_COLOR(color)) {
    gprintf(internal_state, "gnugo: --decide-semeai called on invalid data\n");
    return;
  }

  /* Prepare pattern matcher and reading code. */
  reset_engine(internal_state);

  silent_examine_position(internal_state, EXAMINE_DRAGONS_WITHOUT_OWL);
  gprintf(internal_state, "finished examine_position\n");
  internal_state->count_variations = 1;

  if (*outfilename)
    sgffile_begindump(internal_state, &tree);

  gprintf(internal_state, "Analyzing semeai between %1m and %1m, %C moves first\n",
	  apos, bpos, internal_state->board[apos]);
  owl_analyze_semeai(internal_state, apos, bpos, &resulta, &resultb, &move, &result_certain);
  gprintf(internal_state, "Semeai defense of %1m: result %s %1m\n",
	  apos, result_to_string(resulta), move);
  gprintf(internal_state, "Semeai attack of %1m: result %s %1m\n",
	  bpos, result_to_string(resultb), move);
  gprintf(internal_state, "%d nodes%s\n\n", internal_state->count_variations,
	  result_certain ? "" : ", uncertain result");
  
  gprintf(internal_state, "Analyzing semeai between %1m and %1m, %C moves first\n",
	  bpos, apos, internal_state->board[bpos]);
  owl_analyze_semeai(internal_state, bpos, apos, &resultb, &resulta, &move, &result_certain);
  gprintf(internal_state, "Semeai defense of %1m: result %s %1m\n",
	  bpos, result_to_string(resultb), move);
  gprintf(internal_state, "Semeai attack of %1m: result %s %1m\n",
	  apos, result_to_string(resulta), move);
  gprintf(internal_state, "%d nodes%s\n", internal_state->count_variations,
	  result_certain ? "" : ", uncertain result");

  sgffile_enddump(internal_state, outfilename);
  internal_state->count_variations = 0;
}


/* 
 * decide_position tries to attack and defend every dragon with
 * dragon.escape<6 and writes the variations to an sgf file.
 */

void
decide_position(board_lib_state_struct *internal_state)
{
  int pos;
  int move = NO_MOVE;
  int acode = 0, dcode = 0;
  int kworm;
  static const char *snames[] = {"dead", "alive", "critical", "unknown"};
  SGFTree tree;

  /* Prepare pattern matcher and reading code. */
  reset_engine(internal_state);

  silent_examine_position(internal_state, EXAMINE_DRAGONS_WITHOUT_OWL);

  if (*outfilename)
    sgffile_begindump(internal_state, &tree);

  internal_state->count_variations = 1;

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(internal_state, pos)
	|| dragon[pos].origin != pos
	|| internal_state->board[pos] == EMPTY
	|| DRAGON2(pos).escape_route >= 6)
      continue;

    gprintf(internal_state, "\nanalyzing %1m\n", pos);
    gprintf(internal_state, "status=%s, escape=%d\n", 
	    snames[dragon[pos].crude_status], DRAGON2(pos).escape_route);
    acode = owl_attack(internal_state, pos, &move, NULL, &kworm);
    if (acode) {
      if (acode == WIN) {
	if (move == NO_MOVE)
	  gprintf(internal_state, "%1m is dead as it stands\n", pos);
	else
	  gprintf(internal_state, "%1m can be attacked at %1m (%d variations)\n", 
		  pos, move, internal_state->count_variations);
      }
      else if (acode == KO_A)
	gprintf(internal_state, "%1m can be attacked with ko (good) at %1m (%d variations)\n", 
		pos, move, internal_state->count_variations);
      else if (acode == KO_B)
	gprintf(internal_state, "%1m can be attacked with ko (bad) at %1m (%d variations)\n", 
		pos, move, internal_state->count_variations);
      else if (acode == GAIN)
	gprintf(internal_state, "%1m can be attacked with gain (captures %1m) at %1m (%d variations)", 
		pos, kworm, move, internal_state->count_variations);
      
      internal_state->count_variations = 1;
      dcode = owl_defend(internal_state, pos, &move, NULL, &kworm);
      if (dcode) {
	if (dcode == WIN) {
	  if (move == NO_MOVE)
	    gprintf(internal_state, "%1m is alive as it stands\n", pos);
	  else 
	    gprintf(internal_state, "%1m can be defended at %1m (%d variations)\n", 
		    pos, move, internal_state->count_variations);
	}
	else if (dcode == KO_A)
	  gprintf(internal_state, "%1m can be defended with ko (good) at %1m (%d variations)\n", 
		  pos, move, internal_state->count_variations);
	else if (dcode == KO_B)
	  gprintf(internal_state, "%1m can be defended with ko (bad) at %1m (%d variations)\n",
		  pos, move, internal_state->count_variations);
	else if (dcode == LOSS)
	  gprintf(internal_state, "%1m can be defended with loss (loses %1m) at %1m (%d variations)", 
		  pos, kworm, move, internal_state->count_variations);
      }
      else
	gprintf(internal_state, "%1m cannot be defended (%d variations)\n", 
		pos, internal_state->count_variations);
    }
    else 
      gprintf(internal_state, "%1m cannot be attacked (%d variations)\n", 
	      pos, internal_state->count_variations);
    
    if (acode) {
      if (dcode)
	gprintf(internal_state, "status of %1m revised to CRITICAL\n", pos);
      else
	gprintf(internal_state, "status of %1m revised to DEAD\n", pos);
    }
    else
      gprintf(internal_state, "status of %1m revised to ALIVE\n", pos);
  }
  
  sgffile_enddump(internal_state, outfilename);
  internal_state->count_variations = 0;
}


/*
 * Evaluates the eyespace at (pos) and prints a report. You can get
 * more information by adding -d0x02 to the command line.
 */

void
decide_eye(board_lib_state_struct *internal_state,
           int pos)
{
  int color;
  struct eyevalue value;
  int attack_point;
  int defense_point;
  int eyepos;
  SGFTree tree;

  reset_engine(internal_state);
  silent_examine_position(internal_state, EXAMINE_DRAGONS_WITHOUT_OWL);
  
  color = black_eye[pos].color;
  if (!IS_STONE(color)) {
    gprintf(internal_state, "The eye at %1m is not of a single color.\n", pos);
    return;
  }

  if (printboard)
    showboard(internal_state, 0);

  /* Enable sgf output. */
  if (*outfilename)
    sgffile_begindump(internal_state, &tree);
  internal_state->count_variations = 1;
  
  if (black_eye[pos].color == BLACK) {
    eyepos = black_eye[pos].origin;
    compute_eyes(internal_state, eyepos, &value, &attack_point, &defense_point,
		 black_eye, half_eye, 0);
    gprintf(internal_state, "Black eyespace at %1m: %s\n", eyepos, eyevalue_to_string(&value));
    if (eye_move_urgency(&value) > 0) {
      gprintf(internal_state, "  vital points: %1m (attack) %1m (defense)\n", attack_point,
	      defense_point);
    }
  }
  
  if (white_eye[pos].color == WHITE) {
    eyepos = white_eye[pos].origin;
    compute_eyes(internal_state, eyepos, &value, &attack_point, &defense_point,
		 white_eye, half_eye, 0);
    gprintf(internal_state, "White eyespace at %1m: %s\n", eyepos, eyevalue_to_string(&value));
    if (eye_move_urgency(&value) > 0) {
      gprintf(internal_state, "  vital points: %1m (attack) %1m (defense)\n", attack_point,
	      defense_point);
    }
  }
  
  /* Finish sgf output. */
  sgffile_enddump(internal_state, outfilename);
  internal_state->count_variations = 0;
}


/* 
 * decide_combination tries to find a combination attack for (color) by
 * calling atari_atari().
 */

void
decide_combination(board_lib_state_struct *internal_state,
                   int color)
{
  int attack_move;
  signed char defense_moves[BOARDMAX];
  SGFTree tree;
  int first = 1;
  int pos;

  /* Prepare pattern matcher and reading code. */
  reset_engine(internal_state);

  silent_examine_position(internal_state, EXAMINE_ALL);

  if (*outfilename)
    sgffile_begindump(internal_state, &tree);
  internal_state->count_variations = 1;

  if (atari_atari(internal_state, color, &attack_move, defense_moves, verbose)) {
    gprintf(internal_state, "Combination attack for %C at %1m, defense at ", color,
	    attack_move);
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (ON_BOARD(internal_state, pos) && defense_moves[pos]) {
	if (first)
	  first = 0;
	else
	  gprintf(internal_state, ", ");
	gprintf(internal_state, "%1m", pos);
      }
    }
    gprintf(internal_state, "\n");
  }
  else
    gprintf(internal_state, "No Combination attack for %C\n", color);
  
  sgffile_enddump(internal_state, outfilename);
  internal_state->count_variations = 0;
}


void
decide_surrounded(board_lib_state_struct *internal_state,
                  int pos)
{
  int surround_status;

  if (internal_state->board[pos] == EMPTY) {
    fprintf(stderr, "location must not be empty!\n");
    return;
  }

  /* Prepare pattern matcher and reading code. */
  reset_engine(internal_state);

  silent_examine_position(internal_state, EXAMINE_ALL);
  surround_status = compute_surroundings(internal_state, pos, NO_MOVE, 1, NULL);
  if (surround_status == 1)
    gprintf(internal_state, "the dragon at %1m is SURROUNDED!\n", pos);
  else if (surround_status == 2)
    gprintf(internal_state, "the dragon at %1m is WEAKLY SURROUNDED!\n", pos);
  else
    gprintf(internal_state, "the dragon at %1m is not surrounded.\n", pos);
}  


#if ORACLE

void
decide_oracle(Gameinfo *gameinfo, char *infilename, char *untilstring)
{
  SGFTree tree;

  reset_engine(internal_state);
  if (*outfilename)
    sgffile_begindump(internal_state, &tree);

  internal_state->count_variations = 1;
  summon_oracle();
  oracle_loadsgf(infilename, untilstring);
  consult_oracle(gameinfo->to_move);
  sgffile_enddump(internal_state, outfilename);
  dismiss_oracle();
  internal_state->count_variations = 0;
}

#endif


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */

