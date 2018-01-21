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


/* This file contains functions that deals with threats and, 
 * especially, combinations of threats.
 */

#include "gnugo.h"

#include <string.h>

#include "liberty.h"
#include "gg_utils.h"
#include "patterns.h"


static void find_double_threats(struct board_lib_state_struct *internal_state, int color);

/* Generate move reasons for combination attacks and defenses against
 * them.
 *
 * This is one of the move generators called from genmove().
 */

void
combinations(struct board_lib_state_struct *internal_state, int color)
{
  int save_verbose;
  int attack_point;
  signed char defense_points[BOARDMAX];
  int other = OTHER_COLOR(color);
  int aa_val;

  /* Find intersections with multiple threats. */
  find_double_threats(internal_state, color);

  save_verbose = verbose;
  if (verbose > 0)
    verbose--;

  if (save_verbose)
    gprintf(internal_state, "\nlooking for combination attacks ...\n");
  
  aa_val = atari_atari(internal_state, color, &attack_point, NULL, save_verbose);
  if (aa_val > 0) {
    if (save_verbose)
      gprintf(internal_state, "Combination attack for %C with size %d found at %1m\n",
	      color, aa_val, attack_point);
    add_my_atari_atari_move(internal_state, attack_point, aa_val);
  }
  
  aa_val = atari_atari(internal_state, other, &attack_point, defense_points, save_verbose);
  if (aa_val > 0) {
    int pos;
    if (save_verbose)
      gprintf(internal_state, "Combination attack for %C with size %d found at %1m\n",
	      other, aa_val, attack_point);
    
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (ON_BOARD(internal_state, pos) && defense_points[pos]) {
        add_your_atari_atari_move(internal_state, pos, aa_val);
        if (save_verbose)
          gprintf(internal_state, "- defense at %1m\n", pos);
          }
    }
  }
  verbose = save_verbose;
}


#define MAX_THREATENED_STRINGS  10  /* Should be enough for one intersection */

static void
find_double_threats(struct board_lib_state_struct *internal_state, int color)
{
  int ii;
  int k;
  int l;

  for (ii = BOARDMIN; ii < BOARDMAX; ii++) {
    int num_a_threatened_groups;
    int a_threatened_groups[MAX_THREATENED_STRINGS];
#if 0
    int num_d_threatened_groups;
    int d_threatened_groups[MAX_THREATENED_STRINGS];
#endif

    if (!ON_BOARD(internal_state, ii))
      continue;

    /* Generate an EITHER_MOVE move reasons for each pair of the 
     * threatened strings.  We must also remove the threats, because
     * otherwise we would get followup points for them as well.
     *
     * FIXME: 
     *   - This is perhaps not the best way to do it, but realistically
     *     it will be seldom that more than two strings are threatened
     *     at the same point.  Still, we should find a better way.
     *   - EITHER_MOVE should be generalized to more than two strings.
     */
    num_a_threatened_groups = get_attack_threats(ii, MAX_THREATENED_STRINGS,
						 a_threatened_groups);
    if (num_a_threatened_groups > 1) {
      if (trymove(internal_state, ii, color, "find_double_threats-A", ii)) {
	for (k = 0; k < num_a_threatened_groups - 1; ++k)
	  for (l = k + 1; l < num_a_threatened_groups; ++l) {
	    /* Note: If we used attack_either() here instead of trymove()
	     *       and !defend_both(), we would not make use of the fact
	     *       that we already know of a common threat point for
	     *       the two strings.
	     * Besides, attack_either is currently (3.1.11) not very good.
	     *
	     * The call to attack() is intended to detect the case
	     * where the move at ii is a snapback capture.
	     */
        if (internal_state->board[a_threatened_groups[k]] == EMPTY
        || internal_state->board[a_threatened_groups[l]] == EMPTY) {
          if (!attack(internal_state, ii, NULL)) {
            TRACE(internal_state, "Double threat at %1m, either %1m or %1m attacked.\n",
                  ii, a_threatened_groups[k], a_threatened_groups[l]);
            add_either_move(internal_state, ii, ATTACK_STRING, a_threatened_groups[k],
                    ATTACK_STRING, a_threatened_groups[l]);
            remove_attack_threat_move(internal_state, ii, a_threatened_groups[k]);
            remove_attack_threat_move(internal_state, ii, a_threatened_groups[l]);
	      }
	    }
        else if (!defend_both(internal_state, a_threatened_groups[k],
				  a_threatened_groups[l])) {
          TRACE(internal_state, "Double threat at %1m, either %1m or %1m attacked.\n",
		    ii, a_threatened_groups[k], a_threatened_groups[l]);
          add_either_move(internal_state, ii, ATTACK_STRING, a_threatened_groups[k],
			      ATTACK_STRING, a_threatened_groups[l]);
          remove_attack_threat_move(internal_state, ii, a_threatened_groups[k]);
          remove_attack_threat_move(internal_state, ii, a_threatened_groups[l]);
	    }
	  }
    popgo(internal_state);
      }
    }
  }
  
  
  /* FIXME:
   *   TODO:
   *     - defense threats
   *     - combinations of owl threats and other threats
   *     - combinations of threats to cut and connect
   *     - combinations of breakins into enemy territory
   */
}


/* ================================================================ */
/*                       Combination attacks                        */
/* ================================================================ */


/* atari_atari(internal_state, color, *move) looks for a series of ataris on
 * strings of the other color culminating in the capture of
 * a string which is thought to be invulnerable by the reading
 * code. Such a move can be missed since it may be that each
 * string involved individually can be rescued, but nevertheless
 * one of them can be caught. The simplest example is a double
 * atari. The return value is the size of the smallest opponent
 * worm. 
 *
 * One danger with this scheme is that the first atari
 * tried might be irrelevant to the actual combination.
 * To detect this possibility, once we've found a combination,
 * we mark that first move as forbidden, then try again. If
 * no combination of the same size or larger turns up, then
 * the first move was indeed essential.
 *
 * For the purpose of the move generation, returns the
 * size of the smallest of the worms under attack.
 */

/* Local struct to keep track of atari_atari attack moves and what
 * they threat.
 */
#define AA_MAX_TARGETS_PER_MOVE 4

#define MAX_AA_DIST 5

struct aa_move {
  int move;
  int target[AA_MAX_TARGETS_PER_MOVE];
};

#define AA_MAX_MOVES MAX_BOARD * MAX_BOARD  
static int aa_status[BOARDMAX]; /* ALIVE, DEAD or CRITICAL */
static int forbidden[BOARDMAX];
static int aa_values[BOARDMAX];
static void compute_aa_status(struct board_lib_state_struct *internal_state,
                  int color,
                  const signed char safe_stones[BOARDMAX]);
static void compute_aa_values(struct board_lib_state_struct *internal_state,
                              int color);
static int get_aa_status(struct board_lib_state_struct *internal_state,
                         int pos);
static int do_atari_atari(struct board_lib_state_struct *internal_state,
              int color, int *attack_point, int *defense_point,
              signed char all_potential_defenses[BOARDMAX],
              int last_friendly, int save_verbose, int minsize,
              signed char goal[BOARDMAX]);
static int atari_atari_succeeded(struct board_lib_state_struct *internal_state,
                                int color, int *attack_point,
                                int *defense_point, int last_friendly,
                                int save_verbose, int minsize);
static void atari_atari_find_attack_moves(struct board_lib_state_struct *internal_state,
                      int color, int minsize,
                      struct aa_move attacks[AA_MAX_MOVES],
                      signed char goal[BOARDMAX]);
static void atari_atari_attack_patterns(struct board_lib_state_struct *internal_state,
                    int color, int minsize,
                    struct aa_move attacks[AA_MAX_MOVES],
                    signed char goal[BOARDMAX]);
static void atari_atari_attack_callback(struct board_lib_state_struct *internal_state,
                    int anchor, int color,
                    struct pattern *pattern,
                    int ll, void *data);
static int atari_atari_find_defense_moves(struct board_lib_state_struct *internal_state,
                      int targets[AA_MAX_TARGETS_PER_MOVE],
                      int moves[AA_MAX_MOVES]);
static int get_aa_value(struct board_lib_state_struct *internal_state,
                        int str);
static int update_aa_goal(struct board_lib_state_struct *internal_state,
              signed char goal[BOARDMAX],
              signed char new_goal[BOARDMAX],
              int apos, int color);
static void aa_init_moves(struct aa_move attacks[AA_MAX_MOVES]);
static void aa_add_move(board_lib_state_struct *internal_state,
            struct aa_move attacks[AA_MAX_MOVES],
            int move, int target);
static int aa_move_known(board_lib_state_struct *internal_state,
             struct aa_move attacks[AA_MAX_MOVES],
             int move, int target);
static void aa_sort_moves(board_lib_state_struct *internal_state,
                          struct aa_move attacks[AA_MAX_MOVES]);

/* Set to 1 if you want verbose traces from this function. */

int
atari_atari(board_lib_state_struct *internal_state,
            int color, int *attack_move, signed char defense_moves[BOARDMAX],
	    int save_verbose)
{
  int other = OTHER_COLOR(color);
  int apos;
  int dpos;
  int aa_val;
  signed char saved_defense_moves[BOARDMAX];

  /* Collect worm statuses of opponent's worms. We need to
   * know this because we only want to report unexpected
   * results. For example, we do not want to report success
   * if we find we can kill a worm which is already dead.
   * The worm status of empty points is set to UNKNOWN to signal
   * that stones added along the way need special attention.
   */
  if (aa_depth < 2)
    return 0;
  memset(forbidden, 0, sizeof(forbidden));

  compute_aa_status(internal_state, color, NULL);
  compute_aa_values(internal_state, color);
  
  if (defense_moves)
    memset(defense_moves, 0, BOARDMAX);
  aa_val = do_atari_atari(internal_state, color, &apos, &dpos, defense_moves, NO_MOVE,
			  save_verbose, 0, NULL);

  if (aa_val == 0)
    return 0;

  /* We try excluding the first atari found and see if the
   * combination still works. Repeat until failure.
   */
  while (1) {
    int new_aa_val;
    
    if (attack_move)
      *attack_move = apos;
    
    forbidden[apos] = 1;
    if (defense_moves) {
      memcpy(saved_defense_moves, defense_moves, BOARDMAX);
      memset(defense_moves, 0, BOARDMAX);
    }
    new_aa_val = do_atari_atari(internal_state, color, &apos, &dpos, defense_moves, NO_MOVE,
				save_verbose, aa_val, NULL);

    /* The last do_atari_atari call fails. When do_atari_atari fails,
     * it does not change the value of (apos), so these correspond
     * to a move that works and is necessary.
     */
    if (new_aa_val == 0)
      break;
    else
      aa_val = new_aa_val;
  }

  if (defense_moves) {
    int pos;
    memcpy(defense_moves, saved_defense_moves, BOARDMAX);
    /* defense_moves[] contains potential defense moves. Now we
     * examine which of them really work.
     */
    forbidden[apos] = 0;
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (!ON_BOARD(internal_state, pos) || !defense_moves[pos])
	continue;

      if (!trymove(internal_state, pos, other, "atari_atari", NO_MOVE)) {
	defense_moves[pos] = 0;
	if (save_verbose)
      gprintf(internal_state, "%1m deleted defense point, illegal\n", pos);
	continue;
      }

      if (attack(internal_state, pos, NULL)) {
	defense_moves[pos] = 0;
    popgo(internal_state);
	if (save_verbose)
      gprintf(internal_state, "%1m deleted defense point, unsafe\n", pos);
	continue;
      }
      
      if (do_atari_atari(internal_state, color, &apos, &dpos, NULL, NO_MOVE,
			 save_verbose, aa_val, NULL) > 0) {
	if (save_verbose)
      gprintf(internal_state, "%1m deleted defense point, didn't work\n", pos);
	defense_moves[pos] = 0;
      }
      
      popgo(internal_state);
    }
  }
  return aa_val;
}


/* Wrapper around atari_atari_blunder_size. Check whether a
 * combination attack of size at least minsize appears after move
 * at (move) has been made.
 * The arrays saved_dragons[] and saved_worms[] should be one for
 * stones belonging to dragons or worms respectively, which are
 * supposedly saved by (move).
 *
 * FIXME: We probably want to change the calling convention of this
 *        function to return all defense moves.
 */
int
atari_atari_confirm_safety(board_lib_state_struct *internal_state,
               int color, int move, int *defense, int minsize,
			   const signed char saved_dragons[BOARDMAX],
			   const signed char saved_worms[BOARDMAX])
{
  signed char safe_stones[BOARDMAX];
  signed char defense_moves[BOARDMAX];
  int pos;
  int blunder_size;

  mark_safe_stones(internal_state, color, move, saved_dragons, saved_worms, safe_stones);
  blunder_size = atari_atari_blunder_size(internal_state, color, move, defense_moves,
					  safe_stones);

  if (defense) {
    /* Return one arbitrary defense move. */
    *defense = NO_MOVE;
    for (pos = BOARDMIN; pos < BOARDMAX; pos++)
      if (ON_BOARD(internal_state, pos) && defense_moves[pos]) {
	*defense = pos;
	break;
      }
  }
  
  return blunder_size >= minsize;
}


/* This function checks whether any new combination attack appears after
 * move at (move) has been made, and returns its size (in points).
 * safe_stones marks which of our stones are supposedly safe after this move.
 */
int
atari_atari_blunder_size(struct board_lib_state_struct *internal_state,
             int color, int move,
			 signed char defense_moves[BOARDMAX],
			 const signed char safe_stones[BOARDMAX])
{
  int apos;
  int defense_point = NO_MOVE;
  int aa_val, after_aa_val;
  int other = OTHER_COLOR(color);
  signed char defense_points[BOARDMAX];
  int last_forbidden = NO_MOVE;

  /* If aa_depth is too small, we can't see any combination attacks,
   * so in this respect the move is safe enough.
   */
  if (aa_depth < 2)
    return 0;

  memset(forbidden, 0, sizeof(forbidden));
  memset(defense_points, 0, sizeof(defense_points));

  compute_aa_status(internal_state, other, safe_stones);
  compute_aa_values(internal_state, other);

  /* Accept illegal ko capture here. */
  if (!tryko(internal_state, move, color, NULL))
    /* Really shouldn't happen. */
    abortgo(internal_state, __FILE__, __LINE__, "trymove", move);

  increase_depth_values();

  aa_val = do_atari_atari(internal_state, other, &apos, &defense_point, defense_points,
			  NO_MOVE, 0, 0, NULL);
  after_aa_val = aa_val;

  if (aa_val == 0 || defense_point == NO_MOVE) {

    /* No sufficiently large combination attack, so the move is safe from
     * this danger.
     *
     * On rare occasions do_atari_atari might find a combination
     * but no defense. In this case we assume that the combination
     * is illusory.
     */

    popgo(internal_state);
    decrease_depth_values();
    return 0;
  }

  while (aa_val >= after_aa_val && defense_point != NO_MOVE) {
    /* Try dropping moves from the combination and see if it still
     * works. What we really want is to get the proper defense move
     * into (*defense).
     */
    forbidden[apos] = 1;
    last_forbidden = apos;
    aa_val = do_atari_atari(internal_state, other, &apos, &defense_point, NULL,
			    NO_MOVE, 0, aa_val, NULL);
  }

  popgo(internal_state);
  decrease_depth_values();
  /* We know that a combination exists, but we don't know if
   * the original move at (aa) was really relevant. So we
   * try omitting it and see if a combination is still found.
   */
  compute_aa_status(internal_state, other, NULL);
  compute_aa_values(internal_state, other);
  forbidden[last_forbidden] = 0;
  aa_val = do_atari_atari(internal_state, other, NULL, NULL, NULL, NO_MOVE, 0, 0, NULL);
  if (aa_val >= after_aa_val)
    return 0;

  /* Try the potential defense moves to see which are effective. */
  if (defense_moves) {
    int pos;
    /* defense_points[] contains potential defense moves. Now we
     * examine which of them really work.
     */
    
    /* FIXME: Maybe these should be moved after the tryko() below? */
    compute_aa_status(internal_state, other, safe_stones);
    compute_aa_values(internal_state, other);
      
    memcpy(defense_moves, defense_points, sizeof(defense_points));
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (!ON_BOARD(internal_state, pos) || !defense_moves[pos] || pos == move)
	continue;

      if (!trymove(internal_state, pos, color, "atari_atari", NO_MOVE)) {
	defense_moves[pos] = 0;
	continue;
      }
      increase_depth_values();

      if (attack(internal_state, pos, NULL)) {
	defense_moves[pos] = 0;
	decrease_depth_values();
    popgo(internal_state);
	continue;
      }
      
      /* Accept illegal ko capture here. */
      if (!tryko(internal_state, move, color, NULL))
	/* Really shouldn't happen. */
    abortgo(internal_state, __FILE__, __LINE__, "trymove", move);
      increase_depth_values();
      
      if (do_atari_atari(internal_state, other, &apos, &defense_point, NULL, NO_MOVE,
			 0, after_aa_val, NULL) >= after_aa_val)
	defense_moves[pos] = 0;

      decrease_depth_values();
      popgo(internal_state);
      decrease_depth_values();
      popgo(internal_state);
    }
  }
  
  return after_aa_val - aa_val;
}


/* ---------------------------------------------------------------- */
/*                Helper functions for atari_atari.                 */
/* ---------------------------------------------------------------- */


/* Helper function for computing the aa_status for all opponent's strings.
 * If safe_stones is given, we just copy the information from there.
 * If called at stackp > 0, safe_stones must be provided since the
 * dragon_data is not valid then.
 */

static void
compute_aa_status(struct board_lib_state_struct *internal_state,
                  int color, const signed char safe_stones[BOARDMAX])
{
  int other = OTHER_COLOR(color);
  int pos;
  SGFTree *save_sgf_dumptree = internal_state->sgf_dumptree;
  int save_count_variations = internal_state->count_variations;
  int save_verbose = verbose;

  gg_assert(internal_state, safe_stones || internal_state->stackp == 0);
  
  internal_state->sgf_dumptree = NULL;
  internal_state->count_variations = 0;
  if (verbose)
    verbose--;
  
  /* Collect worm statuses of opponent's worms. We need to
   * know this because we only want to report unexpected
   * results. For example, we do not want to report success
   * if we find we can kill a worm which is already dead.
   * The worm status of empty points is set to UNKNOWN to signal
   * that stones added along the way need special attention.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (internal_state->board[pos] == other) {
      if (safe_stones) {
	if (safe_stones[pos])
	  aa_status[pos] = ALIVE;
	else
	  aa_status[pos] = DEAD;
      }
      else {
	if (dragon[pos].status == DEAD)
	  aa_status[pos] = DEAD;
	else if (dragon[pos].status == CRITICAL)
	  aa_status[pos] = CRITICAL;
	else if (worm[pos].attack_codes[0] != 0) {
	  if (worm[pos].defense_codes[0] != 0)
	    aa_status[pos] = CRITICAL;
	  else
	    aa_status[pos] = DEAD;
	}
	else
	  aa_status[pos] = ALIVE;
      }
    }
    else if (ON_BOARD(internal_state, pos))
      aa_status[pos] = UNKNOWN;
  }
  
  /* reclassify a worm with 2 liberties as INSUBSTANTIAL if capturing
   * it does not result in a live group.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (internal_state->board[pos] == other
    && find_origin(internal_state, pos) == pos
    && countlib(internal_state, pos) == 2
	&& aa_status[pos] == ALIVE) {
      int libs[2];
      findlib(internal_state, pos, 2, libs);
      /* Don't waste time running owl_substantial() if we can't safely
       * atari anyway.
       */
      if (is_self_atari(internal_state, libs[0], color)
      && is_self_atari(internal_state, libs[1], color))
	continue;
      
      if (!owl_substantial(internal_state, pos)) {
	int pos2;
	for (pos2 = BOARDMIN; pos2 < BOARDMAX; pos2++)
      if (internal_state->board[pos2] == other && find_origin(internal_state, pos2) == pos)
	    aa_status[pos2] = INSUBSTANTIAL;
      }
    }
  }
    
  if (debug & DEBUG_ATARI_ATARI) {
    gprintf(internal_state, "compute_aa_status() for %C\n", color);
    gprintf(internal_state, "aa_status: (ALIVE worms not listed)\n");
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (internal_state->board[pos] == other && is_worm_origin(pos, pos)) {
	const char *status = "UNKNOWN (shouldn't happen)";
	if (aa_status[pos] == DEAD)
	  status = "DEAD";
	else if (aa_status[pos] == CRITICAL)
	  status = "CRITICAL";
	else if (aa_status[pos] == INSUBSTANTIAL)
	  status = "INSUBSTANTIAL";
	
	if (aa_status[pos] != ALIVE)
      gprintf(internal_state, "%1M: %s\n", pos, status);
      }
    }
  }

  internal_state->sgf_dumptree = save_sgf_dumptree;
  internal_state->count_variations = save_count_variations;
  verbose = save_verbose;
}


/* Helper function for retrieving the aa_status for a string. We can't
 * reliably do this simply by looking up aa_status[pos] since this is
 * only valid at vertices which were non-empty at the start of the
 * reading. For later added stones, we need to find their aa_status by
 * locating a part of the string which was a worm at the beginning of
 * the reading.
 */

static int
get_aa_status(struct board_lib_state_struct *internal_state, int pos)
{
  int stones[MAX_BOARD * MAX_BOARD];
  int num_stones;
  int k;

  if (aa_status[pos] != UNKNOWN)
    return aa_status[pos];

  num_stones = findstones(internal_state, pos, MAX_BOARD * MAX_BOARD, stones);
  for (k = 0; k < num_stones; k++)
    if (aa_status[stones[k]] != UNKNOWN)
      return aa_status[stones[k]];

  return UNKNOWN;
}



/* Helper function for atari_atari. Here worms is the number of
 * opponent worms involved in the combination, and (last_friendly) is
 * the location of the last friendly move played. Moves marked
 * with the forbidden array are not tried. If no move is found,
 * the values of *attack_point and *defense_point are not changed.
 *
 * If not NULL, *attack_point is left pointing to the location of the
 * attacking move, and *defense_point points to a move defending the
 * combination. In rare cases a defensive move might not be found. If
 * a non-static function calling do_atari_atari gets a return value of
 * 1 but NO_MOVE as the defense point, this should be treated as
 * equivalent to a return value of 0.
 *
 * The goal array limits where we are allowed to consider threats.
 * Only strings for which goal is set to 1 may be threatened. If goal
 * is NULL, anything may be attacked. Thus goal is typically NULL when
 * do_atari_atari() is called from an external function. After the
 * first threat has been made, the goal array is set to one in a
 * neighborhood of the move and after subsequent threats it is
 * expanded with neighborhoods of those moves. The details of this can
 * be found in the function update_aa_goal().
 */

static int
do_atari_atari(struct board_lib_state_struct *internal_state,
           int color, int *attack_point, int *defense_point,
	       signed char all_potential_defenses[BOARDMAX], int last_friendly,
	       int save_verbose, int minsize, signed char goal[BOARDMAX])
{
  int other = OTHER_COLOR(color);
  int k;
  struct aa_move attacks[AA_MAX_MOVES];
  int num_defense_moves;
  int defense_moves[AA_MAX_MOVES];
  int pos;
  SGFTree *save_sgf_dumptree;
  int save_count_variations;
  
  if (debug & DEBUG_ATARI_ATARI) {
    gprintf(internal_state, "%odo_atari_atari: ");
    dump_stack(internal_state);
    gprintf(internal_state, "%oforbidden moves: ");
    for (pos = BOARDMIN; pos < BOARDMAX; pos++)
      if (ON_BOARD(internal_state, pos) && forbidden[pos])
    gprintf(internal_state, "%o%1m ", pos);
    gprintf(internal_state, "\n");
    gprintf(internal_state, "%ogoal: ");
    if (!goal)
      gprintf(internal_state, "none");
    else {
      for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(internal_state, pos) && goal[pos])
      gprintf(internal_state, "%o%1m ", pos);
    }
    gprintf(internal_state, "\n");
  }

  /* First look for strings adjacent to the last friendly move played
   * (or to another stone in the same string) which can be
   * unexpectedly attacked.  If so, the combination attack
   * has succeeded.
   */
  if (last_friendly != NO_MOVE) {
    int retval;
    save_sgf_dumptree = internal_state->sgf_dumptree;
    save_count_variations = internal_state->count_variations;
    internal_state->sgf_dumptree = NULL;
    internal_state->count_variations = 0;
    retval = atari_atari_succeeded(internal_state, color, attack_point, defense_point,
				   last_friendly, save_verbose, minsize);
    internal_state->sgf_dumptree = save_sgf_dumptree;
    internal_state->count_variations = save_count_variations;
    if (retval != 0) {
      if (internal_state->sgf_dumptree)
	/* FIXME: Better message. */
    sgftreeAddComment(internal_state->sgf_dumptree, "attack found");
      return retval;
    }
  }

  if (internal_state->stackp > aa_depth)
    return 0;

  /* Find attack moves. These are typically ataris but may also be
   * more general.
   */
  save_sgf_dumptree = internal_state->sgf_dumptree;
  save_count_variations = internal_state->count_variations;
  internal_state->sgf_dumptree = NULL;
  internal_state->count_variations = 0;
  atari_atari_find_attack_moves(internal_state, color, minsize, attacks, goal);
  internal_state->sgf_dumptree = save_sgf_dumptree;
  internal_state->count_variations = save_count_variations;

  /* Try the attacking moves and let the opponent defend. Then call
   * ourselves recursively.
   */
  for (k = 0; attacks[k].move != NO_MOVE; k++) {
    int aa_val;
    int str = attacks[k].target[0];
    int apos = attacks[k].move;
    int bpos;
    int r;
    
    if (!trymove(internal_state, apos, color, "do_atari_atari-A", str))
      continue;
    
    if (all_potential_defenses) {
      all_potential_defenses[apos] = 1;
      if (countlib(internal_state, apos) <= 2) {
	int libs[2];
    int num_libs = findlib(internal_state, apos, 2, libs);
	all_potential_defenses[libs[0]] = 1;
	if (num_libs == 2)
	  all_potential_defenses[libs[1]] = 1;
      }
    }

    if (!IS_STONE(internal_state->board[str])) {
      /* Error situation. This could be caused by a wrong matcher status. */
      if (save_verbose || (debug & DEBUG_ATARI_ATARI))
    gprintf(internal_state, "%oError condition found by atari_atari\n");
      popgo(internal_state);
      return 0;
    }

    /* Try to defend the stone (str) which is threatened. */
    aa_val = get_aa_value(internal_state, str);

    /* Pick up defense moves. */
    save_sgf_dumptree = internal_state->sgf_dumptree;
    save_count_variations = internal_state->count_variations;
    internal_state->sgf_dumptree = NULL;
    internal_state->count_variations = 0;
    num_defense_moves = atari_atari_find_defense_moves(internal_state, attacks[k].target,
						       defense_moves);
    internal_state->sgf_dumptree = save_sgf_dumptree;
    internal_state->count_variations = save_count_variations;
    
    for (r = 0; r < num_defense_moves; r++) {
      bpos = defense_moves[r];

      if (all_potential_defenses)
	all_potential_defenses[bpos] = 1;

      if (trymove(internal_state, bpos, other, "do_atari_atari-B", str)) {
	int new_aa_val;
	signed char new_goal[BOARDMAX];
	/* These moves may have been irrelevant for later
	 * reading, so in order to avoid horizon problems, we
	 * need to temporarily increase the depth values.
	 */
	modify_depth_values(2);
    update_aa_goal(internal_state, goal, new_goal, apos, color);
    new_aa_val = do_atari_atari(internal_state, color, NULL, defense_point,
				    all_potential_defenses,
				    apos, save_verbose, minsize, new_goal);
	modify_depth_values(-2);
	if (new_aa_val < aa_val)
	  aa_val = new_aa_val;
    popgo(internal_state);
      }

      /* Defense successful, no need to try any further. */
      if (aa_val == 0)
	break;
    }

    /* Undo the attacking move. */
    popgo(internal_state);

    if (aa_val == 0)
      continue;

    /* atari_atari successful */
    if (num_defense_moves == 0) {
      if (save_verbose || (debug & DEBUG_ATARI_ATARI)) {
    gprintf(internal_state, "%oThe worm %1m can be attacked at %1m after ", str, apos);
    dump_stack(internal_state);
      }
      if (internal_state->sgf_dumptree)
	/* FIXME: Better message. */
    sgftreeAddComment(internal_state->sgf_dumptree, "attack found");
    }

    if (attack_point)
      *attack_point = apos;

    if (defense_point) {
      save_sgf_dumptree = internal_state->sgf_dumptree;
      save_count_variations = internal_state->count_variations;
      internal_state->sgf_dumptree = NULL;
      internal_state->count_variations = 0;

      if (!find_defense(internal_state, str, defense_point))
	*defense_point = NO_MOVE;

      /* If no defense point is known and (apos) is a safe
       * move for other, it probably defends the combination.
       */
      if ((*defense_point == NO_MOVE || !safe_move(internal_state, *defense_point, other))
      && safe_move(internal_state, apos, other))
	*defense_point = apos;

      internal_state->sgf_dumptree = save_sgf_dumptree;
      internal_state->count_variations = save_count_variations;
    }
    
    DEBUG(internal_state, DEBUG_ATARI_ATARI, "%oreturn value:%d (%1m)\n", aa_val, str);
    return aa_val;
  }
    
  /* No atari_atari attack. */
  return 0;
}


static int
atari_atari_succeeded(struct board_lib_state_struct *internal_state,
              int color, int *attack_point, int *defense_point,
		      int last_friendly, int save_verbose, int minsize)
{
  int pos;
  int apos;
  int other = OTHER_COLOR(color);

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (internal_state->board[pos] != other)
      continue;
    
    if (pos != find_origin(internal_state, pos))
      continue;
    
    if (minsize > 0
    && get_aa_value(internal_state, pos) < minsize)
      continue;
    
    if (get_aa_status(internal_state, pos) != ALIVE)
      continue;
    
    if (internal_state->board[last_friendly] != EMPTY
    && !adjacent_strings(internal_state, last_friendly, pos))
      continue;
    
    if (internal_state->board[last_friendly] == EMPTY
    && !liberty_of_string(internal_state, last_friendly, pos))
      continue;
    
    if (debug & DEBUG_ATARI_ATARI)
      gprintf(internal_state, "Considering attack of %1m. depth = %d.\n", pos, depth);
    
    if (attack(internal_state, pos, &apos) && !forbidden[apos]) {
      if (save_verbose || (debug & DEBUG_ATARI_ATARI)) {
    gprintf(internal_state, "%oThe worm %1m can be attacked at %1m after ", pos, apos);
    dump_stack(internal_state);
      }
      if (attack_point)
	*attack_point = apos;
      
      /* We look for a move defending the combination.
       * Normally this is found by find_defense but failing
       * that, if the attacking move is a safe move for color,
       * it probably defends.
       */
      if (defense_point) {
    if (!find_defense(internal_state, pos, defense_point)) {
      if (safe_move(internal_state, apos, other))
	    *defense_point = apos;
	  else
	    *defense_point = NO_MOVE;
	}
      }
      
      DEBUG(internal_state, DEBUG_ATARI_ATARI, "%oreturn value:%d (%1m)\n",
        get_aa_value(internal_state, pos), pos);
      return get_aa_value(internal_state, pos);
    }
  }
  
  return 0;
}

#define MAX_THREAT_MOVES  MAX_TACTICAL_POINTS

static void
atari_atari_find_attack_moves(struct board_lib_state_struct *internal_state,
                  int color, int minsize,
			      struct aa_move attacks[AA_MAX_MOVES],
			      signed char goal[BOARDMAX])
{
  int k;
  int r;

  aa_init_moves(attacks);

  atari_atari_attack_patterns(internal_state, color, minsize, attacks, goal);

  /* Sort the attack moves. */
  aa_sort_moves(internal_state, attacks);
  
  if (debug & DEBUG_ATARI_ATARI) {
    gprintf(internal_state, "Attack moves:");
    for (k = 0; k < AA_MAX_MOVES && attacks[k].move != NO_MOVE; k++) {
      gprintf(internal_state, "%o %1m(", attacks[k].move);
      for (r = 0; r < AA_MAX_TARGETS_PER_MOVE; r++) {
	if (attacks[k].target[r] == NO_MOVE)
	  break;
    gprintf(internal_state, "%o%s%1m", r == 0 ? "" : ",", attacks[k].target[r]);
      }
      gprintf(internal_state, "%o)");
    }
    gprintf(internal_state, "%o\n");
  }
}

/* FIXME: Move these to a struct and pass to callback through the
 * *data parameter.
 */
static int current_minsize;
static struct aa_move *current_attacks;
static int conditional_attack_point[BOARDMAX];

static void
atari_atari_attack_patterns(struct board_lib_state_struct *internal_state,
                int color, int minsize,
			    struct aa_move attacks[AA_MAX_MOVES],
			    signed char goal[BOARDMAX])
{
  signed char revised_goal[BOARDMAX];
  current_minsize = minsize;
  current_attacks = attacks;
  memset(conditional_attack_point, 0, sizeof(conditional_attack_point));

  /* If goal is NULL and there are forbidden moves we need to compute
   * a new goal around the forbidden moves.
   */
  if (goal == NULL && update_aa_goal(internal_state, goal, revised_goal, NO_MOVE, color))
    goal = revised_goal;

#if 0
  if (goal != NULL) {
    int pos;
    gprintf(internal_state, "goal:");
    for (pos = BOARDMIN; pos < BOARDMAX; pos++)
      if (ON_BOARD(internal_state, pos) && goal[pos])
    gprintf(internal_state, "%o %1m", pos);
    gprintf(internal_state, "%o\n");
  }
#endif
  
  matchpat(internal_state, atari_atari_attack_callback, color, &aa_attackpat_db, NULL, goal);
}

/* Try to attack every X string in the pattern, whether there is an attack
 * before or not. Only exclude already known attacking moves.
 */
static void
atari_atari_attack_callback(struct board_lib_state_struct *internal_state,
                int anchor, int color,
			    struct pattern *pattern, int ll, void *data)
{
  int move;
  int k;
  UNUSED(data);

  move = AFFINE_TRANSFORM(pattern->move_offset, ll, anchor);

  if (forbidden[move])
    return;
  
  /* If the pattern has a constraint, call the autohelper to see
   * if the pattern must be rejected.
   */
  if (pattern->autohelper_flag & HAVE_CONSTRAINT)
    if (!pattern->autohelper(internal_state, ll, move, color, 0))
      return;

  /* If the pattern has a helper, call it to see if the pattern must
   * be rejected.
   */
  if (pattern->helper)
    if (!pattern->helper(internal_state, pattern, ll, move, color))
      return;

  /* Loop through pattern elements in search of X strings to
   * threaten to attack.
   */
  for (k = 0; k < pattern->patlen; ++k) { /* match each point */
    if (pattern->patn[k].att == ATT_X) {
      /* transform pattern real coordinate */
      int str = find_origin(internal_state, AFFINE_TRANSFORM(pattern->patn[k].offset,
					     ll, anchor));

      if (current_minsize > 0
      && get_aa_value(internal_state, str) < current_minsize)
	continue;

      if (aa_move_known(internal_state, current_attacks, move, str))
	continue;

      if (get_aa_status(internal_state, str) != ALIVE)
	continue;

      /* Usually we don't want to play self atari. However, if we
       * capture in snapback it's okay. For s class patterns we don't
       * have this requirement.
       */
      if (!(pattern->class & CLASS_s) && is_self_atari(internal_state, move, color)) {
    if (countlib(internal_state, str) > 2)
	  continue;

    if (!safe_move(internal_state, move, color))
	  continue;
      }
      
      /*
       * Play (move) and see if there is an attack.
       */
      if (trymove(internal_state, move, color, "attack_callback", str)) {
	int acode;
	int attack_point = NO_MOVE;

    if (!internal_state->board[str])
	  acode = WIN;
	else
      acode = attack(internal_state, str, &attack_point);

    popgo(internal_state);

	if (acode != 0) {
	  if ((pattern->class & CLASS_c)
          && !aa_move_known(internal_state, current_attacks, move, NO_MOVE)) {
	    /* Conditional pattern. */
        DEBUG(internal_state, DEBUG_ATARI_ATARI,
		  "aa_attack pattern %s+%d (conditional) found threat on %1m at %1m with code %d\n",
		  pattern->name, ll, str, move, acode);
	    if (conditional_attack_point[move] == NO_MOVE)
	      conditional_attack_point[move] = str;
	    else if (conditional_attack_point[move] != str) {
          aa_add_move(internal_state, current_attacks, move,
			  conditional_attack_point[move]);
          aa_add_move(internal_state, current_attacks, move, str);
	    }
	  }
	  else {
        aa_add_move(internal_state, current_attacks, move, str);
        DEBUG(internal_state, DEBUG_ATARI_ATARI,
		  "aa_attack pattern %s+%d found threat on %1m at %1m with code %d\n",
		  pattern->name, ll, str, move, acode);
	  }
	}
      }
    }
  }
}


static int
atari_atari_find_defense_moves(struct board_lib_state_struct *internal_state,
                   int targets[AA_MAX_TARGETS_PER_MOVE],
			       int moves[AA_MAX_MOVES])
{
  int num_moves = 0;
  int move;
  int k;
  int liberties;
  int libs[4];
  int neighbors;
  int adjs[MAXCHAIN];
  int mx[BOARDMAX];
  int r, s;

  memset(mx, 0, sizeof(mx));

  for (r = 0; r < AA_MAX_TARGETS_PER_MOVE && targets[r] != NO_MOVE; r++) {
    int str = targets[r];

    /* If the attack move happened to remove (str), there's no defense. */
    if (internal_state->board[str] == EMPTY)
      continue;
    
    /* Because we know (str) is threatened there is an
     * attack and we can be sure find_defense() will give a
     * useful defense point if it returns non-zero. Usually we
     * would need to call attack_and_defend() to be certain of
     * this.
     */
    if (!find_defense(internal_state, str, &move))
      continue;
    moves[num_moves++] = move;
    if (num_moves == AA_MAX_MOVES)
      return num_moves;
    mx[move] = 1;
    
    /* Consider all moves to attack a neighbor or to play on a liberty. */
    liberties = findlib(internal_state, str, 4, libs);
    for (k = 0; k < liberties; k++) {
      if (!mx[libs[k]]
      && trymove(internal_state, libs[k], internal_state->board[str], "aa_defend-A", str)) {
    if (attack(internal_state, str, NULL) == 0) {
	  moves[num_moves++] = libs[k];
	  mx[libs[k]] = 1;
	}
    popgo(internal_state);
	if (num_moves == AA_MAX_MOVES)
	  return num_moves;
      }
    }
    
    neighbors = chainlinks(internal_state, str, adjs);
    for (k = 0; k < neighbors; k++) {
      int attack_point;
      if (attack(internal_state, adjs[k], &attack_point) == WIN
	  && !mx[attack_point]) {
	moves[num_moves++] = attack_point;
	if (num_moves == AA_MAX_MOVES)
	  return num_moves;
	mx[attack_point] = 1;
      }

      /* If the neighbor has at most three liberties, try all of them
       * for defense, except self-ataris.
       */
      liberties = findlib(internal_state, adjs[k], 3, libs);
      if (liberties <= 3) {
	for (s = 0; s < liberties; s++) {
	  if (!mx[libs[s]]
          && !is_self_atari(internal_state, libs[s], internal_state->board[str])
          && trymove(internal_state, libs[s], internal_state->board[str], "aa_defend-B", str)) {
        if (attack(internal_state, str, NULL) == 0) {
	      moves[num_moves++] = libs[s];
	      mx[libs[s]] = 1;
	    }
        popgo(internal_state);
	    if (num_moves == AA_MAX_MOVES)
	      return num_moves;
	  }
	}
      }
    }
    
    if (debug & DEBUG_ATARI_ATARI) {
      gprintf(internal_state, "Defense moves for %1m:", str);
      for (k = 0; k < num_moves; k++)
    gprintf(internal_state, "%o %1m", moves[k]);
      gprintf(internal_state, "%o\n");
    }
  }

  return num_moves;
}


/* Try to guess the value of the strings. We do this by adding twice
 * the number of stones to the number of liberties and second order
 * liberties within the moyo around the string. This is of course
 * quite crude since it doesn't take into account any strategic
 * effects, e.g. a string being cutting stones. 
 */
static void
compute_aa_values(struct board_lib_state_struct *internal_state, int color)
{
  int other = OTHER_COLOR(color);
  int pos;
  int value;
  int liberties;
  int libs[MAXLIBS];
  int mx[BOARDMAX];
  int r, k;

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (internal_state->board[pos] != other
    || pos != find_origin(internal_state, pos)
	|| aa_status[pos] != ALIVE) {
      aa_values[pos] = 0;
      continue;
    }
      
    memset(mx, 0, sizeof(mx));
    liberties = findlib(internal_state, pos, MAXLIBS, libs);
    value = 2 * countstones(internal_state, pos);

    for (r = 0; r < liberties; r++) {
      if (!mx[libs[r]]
      && (whose_moyo(internal_state, &initial_black_influence, libs[r]) == other
          || whose_moyo(internal_state, &initial_white_influence, libs[r]) == other)) {
	mx[libs[r]] = 1;
	value++;
      }
      for (k = 0; k < 4; k++) {
	int librd = libs[r] + delta[k];
    if (!ON_BOARD1(internal_state, librd) || mx[librd])
	  continue;
	mx[librd] = 1;
    if (internal_state->board[librd] == EMPTY
        && (whose_moyo(internal_state, &initial_black_influence, librd) == other
        || (whose_moyo(internal_state, &initial_white_influence, librd) == other)))
	  value++;
      }
    }

    aa_values[pos] = value;
    if (1)
      DEBUG(internal_state, DEBUG_ATARI_ATARI, "aa_value for %1m = %d\n", pos, value);
  }
}

/* The aa_value for a string is the sum of the aa_values for all
 * included strings in the original position. This will systematically
 * overvalue strings which consist of multiple original strings, but
 * this is okay since the defender very rarely should defend a string
 * first and then sacrifice it later.
 */
static int
get_aa_value(struct board_lib_state_struct *internal_state, int str)
{
  int stones[MAX_BOARD * MAX_BOARD];
  int k;
  int num_stones = findstones(internal_state, str, MAX_BOARD * MAX_BOARD, stones);
  int value = 0;
  
  for (k = 0; k < num_stones; k++)
    value += aa_values[stones[k]];

  return value;
}


/* update_aa_goal(internal_state, goal, new_goal, apos, color) extends the goal array
 * with vertices in a neighborhood of apos. The algorithm is that
 * starting at apos, a distance measure is computed to nearby
 * vertices. The distance increases with one for each step through
 * empty vertices and by a liberty depending number when passing
 * through strings of the attacked color. Strings with 3 or fewer
 * liberties are free to pass through while strings with more
 * liberties cost (libs - 3) to pass through. Stones with a distance
 * of 5 or less are included in the goal.
 *
 * Additionally neighborhoods of the moves in the forbidden array are
 * included in the goal, to make it possible to limit the goal to a
 * specific area from the beginning. This is needed when trying to
 * decide which moves are relevant to the combination.
 */

#define ENQUEUE(pos, dist) \
    do { \
      if ((dist) <= MAX_AA_DIST) { \
        if (dists[pos] == 0) { \
          queue[queue_end++] = (pos); \
          dists[pos] = (dist); \
        } \
        else if (dists[pos] < (dist)) \
          dists[pos] = (dist); \
      } \
    } while (0);

static int
update_aa_goal(struct board_lib_state_struct *internal_state,
           signed char goal[BOARDMAX], signed char new_goal[BOARDMAX],
	       int apos, int color)
{
  int other = OTHER_COLOR(color);
  int dists[BOARDMAX];
  int queue[MAX_BOARD * MAX_BOARD];
  int queue_end = 0;
  int k, r, s;
  int pos;
  
  if (goal == NULL)
    memset(new_goal, 0, BOARDMAX);
  else
    memcpy(new_goal, goal, BOARDMAX);

  memset(dists, 0, sizeof(dists));

  if (apos != NO_MOVE) {
    dists[apos] = 1;
    queue[queue_end++] = apos;
  }

#if 0
  /* Disabled for now, since it does nothing but break atari_atari:16
   * and trevorc:1540. It could be reactivated when the rest of the
   * function would be modified in order to garanty that a forbidden
   * move is strictly equivalent to a played move in terms of goal
   * mapping. I doubt it would be anything worth though...
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (ON_BOARD(internal_state, pos) && forbidden[pos]) {
      dists[pos] = 1;
      queue[queue_end++] = pos;
    }
  }
#endif

  if (queue_end == 0)
    return 0;
  
  for (r = 0; r < queue_end; r++) {
    int smallest_dist = MAX_BOARD * MAX_BOARD;
    int best_index = -1;

    gg_assert(internal_state, queue_end < MAX_BOARD * MAX_BOARD);
    
    for (k = r; k < queue_end; k++) {
      if (dists[queue[k]] < smallest_dist) {
	smallest_dist = dists[queue[k]];
	best_index = k;
      }
    }
    
    if (best_index != r) {
      int tmp = queue[r];
      queue[r] = queue[best_index];
      queue[best_index] = tmp;
    }
    
    pos = queue[r];
    if (internal_state->board[pos] == other)
      new_goal[pos] = 1;

    /* FIXME: We shouldn't let dead opponent stones stop the
     * propagation of distance.
     *
     * As a partial fix we include pos == apos in a test below.
     */
    for (k = 0; k < 4; k++) {
      int pos2 = pos + delta[k];
      if (!ON_BOARD(internal_state, pos2))
       continue;
      if ((internal_state->board[pos] != color || pos == apos) && internal_state->board[pos2] == EMPTY) {
        ENQUEUE(pos2, dists[pos] + 1);
      }
      else if (internal_state->board[pos] != other && internal_state->board[pos2] == other) {
	int stones[MAX_BOARD * MAX_BOARD];
    int size = findstones(internal_state, pos2, MAX_BOARD * MAX_BOARD, stones);
    int libs = countlib(internal_state, pos2);
	int deltadist = libs - 3;
	if (deltadist < 0)
	  deltadist = 0;
	for (s = 0; s < size; s++)
	  ENQUEUE(stones[s], dists[pos] + deltadist);
      }
    }
  }
  return 1;
}

/* Initialize an array with atari_atari attacks. The convention is that
 * the array ends when a NO_MOVE is encountered in the move field.
 */
static void
aa_init_moves(struct aa_move attacks[AA_MAX_MOVES])
{
  attacks[0].move = NO_MOVE;
}


/* Add an atari_atari attack move to a struct aa_move array. If the
 * move already is included in the array, we check whether the target
 * also is known for that move and add it if not.
 */
static void
aa_add_move(struct board_lib_state_struct *internal_state,
            struct aa_move attacks[AA_MAX_MOVES], int move, int target)
{
  int k;
  int r;

  for (k = 0; k < AA_MAX_MOVES; k++)
    if (attacks[k].move == move || attacks[k].move == NO_MOVE)
      break;

  /* If the array is full, give up. */
  if (k == AA_MAX_MOVES)
    return;

  target = find_origin(internal_state, target);

  /* New move. */
  if (attacks[k].move == NO_MOVE) {
    attacks[k].move = move;
    attacks[k].target[0] = target;
    if (AA_MAX_TARGETS_PER_MOVE > 0)
      attacks[k].target[1] = NO_MOVE;

    if (k < AA_MAX_MOVES - 1)
      attacks[k+1].move = NO_MOVE;

    return;
  }

  /* Known move, maybe new target. */
  for (r = 0; r < AA_MAX_TARGETS_PER_MOVE; r++)
    if (attacks[k].target[r] == target || attacks[k].target[r] == NO_MOVE)
      break;

  /* No place for more targets. */
  if (r == AA_MAX_TARGETS_PER_MOVE)
    return;

  /* Target known. */
  if (attacks[k].target[r] == target)
    return;

  /* Add target. */
  attacks[k].target[r] = target;
  if (r < AA_MAX_TARGETS_PER_MOVE - 1)
    attacks[k].target[r + 1] = NO_MOVE;
}

/* Check whether an atari_atari attack move is included in an struct
 * aa_move array. If target is not NO_MOVE, we also require that the
 * target is known for the move.
 */
static int
aa_move_known(struct board_lib_state_struct *internal_state,
              struct aa_move attacks[AA_MAX_MOVES], int move, int target)
{
  int k;
  int r;

  for (k = 0; k < AA_MAX_MOVES; k++)
    if (attacks[k].move == move || attacks[k].move == NO_MOVE)
      break;

  /* If the array is full, give up and claim the move to be known. */
  if (k == AA_MAX_MOVES)
    return 1;

  /* Unknown move. */
  if (attacks[k].move == NO_MOVE)
    return 0;

  /* Move known, but how about the target?
   * If no target specified, just return 1.
   */
  if (target == NO_MOVE)
    return 1;

  target = find_origin(internal_state, target);
  for (r = 0; r < AA_MAX_TARGETS_PER_MOVE; r++)
    if (attacks[k].target[r] == target || attacks[k].target[r] == NO_MOVE)
      break;

  /* No place for more targets. Give up and claim the target to be known. */
  if (r == AA_MAX_TARGETS_PER_MOVE)
    return 1;

  /* Target known. */
  if (attacks[k].target[r] == target)
    return 1;

  /* Unknown target. */
  return 0;
}


/* Auxiliary function for aa_sort_moves(). */
static int
target_comp_func(struct board_lib_state_struct *internal_state,
                 const void *a, const void *b)
{
  int asize = get_aa_value(internal_state, *((const int *) a));
  int bsize = get_aa_value(internal_state, *((const int *) b));
  return asize - bsize;
}


/* Auxiliary function for aa_sort_moves(). */
static int
move_comp_func(struct board_lib_state_struct *internal_state,
               const void *a, const void *b)
{
  const struct aa_move *aa = a;
  const struct aa_move *bb = b;
  int asize = get_aa_value(internal_state, aa->target[0]);
  int bsize = get_aa_value(internal_state, bb->target[0]);
  return asize - bsize;
}

/* Sort the attack moves. For each move the targets are sorted in
 * decreasing size. Then the moves are sorted with increasing size
 * of their first target.
 */
static void
aa_sort_moves(board_lib_state_struct *internal_state,
              struct aa_move attacks[AA_MAX_MOVES])
{
  int k;
  int r;
  int number_of_attacks;
  int number_of_targets;

  for (k = 0; k < AA_MAX_MOVES && attacks[k].move != NO_MOVE; k++) {
    for (r = 0; r < AA_MAX_TARGETS_PER_MOVE; r++)
      if (attacks[k].target[r] == NO_MOVE)
	break;
    number_of_targets = r;
    gg_sort(internal_state, attacks[k].target, number_of_targets,
        sizeof(attacks[k].target[0]), target_comp_func);
  }
  number_of_attacks = k;
  gg_sort(internal_state, attacks, number_of_attacks, sizeof(attacks[0]), move_comp_func);
}


#if 0

/* Returns true if a move by (color) at (pos) is atari on something.
 * Currently unused.
 */

static int
is_atari(int pos, int color)
{
  int other = OTHER_COLOR(color);

  if (!is_legal(internal_state, pos, color))
    return 0;
  
  if (internal_state->board[SOUTH(pos)] == other
      && countlib(SOUTH(pos)) == 2)
    return 1;
  
  if (internal_state->board[WEST(pos)] == other
      && countlib(WEST(pos)) == 2)
    return 1;
  
  if (internal_state->board[NORTH(pos)] == other
      && countlib(NORTH(pos)) == 2)
    return 1;
  
  if (internal_state->board[EAST(pos)] == other
      && countlib(EAST(pos)) == 2)
    return 1;
  
  return 0;
}

#endif


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
