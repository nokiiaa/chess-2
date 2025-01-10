#include "move_picker.hh"
#include "board.hh"
#include "bulk_movegen.hh"
#include "lut.hh"
#include "tt.hh"
#include <mutex>

FORCEINLINE move select_move(move *moves, size_t idx, size_t n, int *scores) {
  size_t mxi = idx;

  for (size_t i = idx + 1; i < n; i++)
    if (scores[mxi] < scores[i])
      mxi = i;

  std::swap(moves[mxi], moves[idx]);
  std::swap(scores[mxi], scores[idx]);
  return moves[idx];
}

FORCEINLINE move select_move(move *moves, size_t idx) { return moves[idx]; }

FORCEINLINE bool ttable_probe(board &b, move m, score &s) {
  b.make_move(m);

  auto &ent = ttable::global[b.hash];

  if (ent.hash == b.hash) {
    b.unmake_move();

    s = ent.value;
    return true;
  }

  b.unmake_move();

  return false;
}

move move_picker::next(score alpha, score beta) {
  move *km = bd.appended_moves + 1 < board::max_killer_plies
                 ? bd.killer_moves[bd.appended_moves + 1]
                 : 0;

  switch (phase) {
  // Get hash move
  case 0: {
    phase++;

    if (!tactical_only) {
      std::lock_guard<spinlock> lock(ttable::global_lock);
      const auto &ent = ttable::global[bd.hash];

      if (ent.hash == bd.hash)
        return ent.move;
    }
  }
    [[fallthrough]];
  // Generate tactical moves
  case 1: {
    i_tactical = 0;
    move *end = gen.gen_promotions_captures(tactical_moves);
    num_tactical_moves = end - tactical_moves;

    for (size_t i = 0; i < num_tactical_moves; i++) {
      static const int piece_values[] = {0, 100, 0, 400, 420, 500, 1200, 0};

      score s;

      volatile move tm = tactical_moves[i];

      int tosq = MOVE_TO(tm), fmsq = MOVE_FROM(tm);

      // Use transposition table for move ordering
      if (std::lock_guard lock(ttable::global_lock);
          ttable_probe(bd, tactical_moves[i], s) && s >= alpha)
        tactical_scores[i] = 1000000 + s;
      // This is a typical capture
      else if (bd.pieces[tosq])
        tactical_scores[i] = piece_values[PTYPE(bd.pieces[tosq])] -
                             piece_values[PTYPE(bd.pieces[fmsq])];
      // Promotions are really good
      else
        tactical_scores[i] = 100000;
    }

    phase++;
  }
    [[fallthrough]];
  // Pick tactical moves
  case 2:
    if (i_tactical < num_tactical_moves)
      return sort_moves ? select_move(tactical_moves, i_tactical++,
                                      num_tactical_moves, tactical_scores)
                        : select_move(tactical_moves, i_tactical++);
    else if (tactical_only)
      return 0;
    else
      phase++;
    [[fallthrough]];
  // Generate killer moves
  case 3:
    phase++;
    if (km && km[0] && bulk_move_generator::is_move_pseudolegal(bd, km[0]))
      return km[0];
    [[fallthrough]];
  case 4:
    phase++;
    if (km && km[1] && bulk_move_generator::is_move_pseudolegal(bd, km[1]))
      return km[1];
    [[fallthrough]];
  // Generate quiet moves
  case 5: {
    i_quiet = 0;
    move *end = gen.gen_noncaptures(quiet_moves);
    num_quiet_moves = end - quiet_moves;

    for (size_t i = 0; i < num_quiet_moves; i++) {
      score s;
      move quiet = quiet_moves[i];
      if (std::lock_guard lock(ttable::global_lock);
          ttable_probe(bd, quiet, s) && s >= alpha)
        quiet_scores[i] = 1000000 + s;
      else
        quiet_scores[i] = bd.history_table[PTYPE(bd.pieces[MOVE_FROM(quiet)])]
                                          [MOVE_TO(quiet)];
    }

    phase++;
  }
    [[fallthrough]];
  // Pick quiet moves
  case 6:
    if (i_quiet < num_quiet_moves)
      return sort_moves ? select_move(quiet_moves, i_quiet++, num_quiet_moves,
                                      quiet_scores)
                        : select_move(quiet_moves, i_quiet++);
    else
      phase++;
    [[fallthrough]];
  default:
    return 0;
  }
}