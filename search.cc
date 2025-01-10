#include "search.hh"
#include "board.hh"
#include "move_picker.hh"
#include "tt.hh"
#include <algorithm>
#include <mutex>
#include <sstream>
#include <thread>

constexpr auto evfunc = eval::myeval;

std::atomic<size_t> search::branches = 0, search::searches = 0,
                    search::nodes = 0;

score search::quiescence(board &board, score alpha, score beta, int depth,
                         const execution_limiter &el) {
  // nodes++;

  if (el.stop.load())
    throw out_of_time_exception();

  if (depth == 0)
    return evfunc(board);

  const int stand_pat = evfunc(board);

  if (stand_pat >= beta)
    return beta;

  const int delta = 1200;

  if (alpha > stand_pat + delta)
    return alpha;

  if (alpha < stand_pat)
    alpha = stand_pat;

  // searches++;

  move_picker pick(board, true, true);

  score best = stand_pat;

  while (move m = pick.next(alpha, beta)) {
    board.make_move(m);
    board.appended_moves++;

    if (!board.in_check(board.side_to_move ^ 1)) {
      // branches++;

      score scr;
      scr = -quiescence(board, -beta, -alpha, depth - 1, el);

      if (scr >= beta) {
        board.unmake_move();
        board.appended_moves--;
        return scr;
      }

      if (scr > best)
        best = scr;

      if (scr > alpha)
        alpha = scr;
    }

    board.unmake_move();
    board.appended_moves--;
  }

  return best;
}

score search::perform(board &board, score alpha, score beta, int depth,
                      move *best, const execution_limiter &el) {
  nodes++;

  score org_alpha = alpha;

  if (el.stop.load())
    throw out_of_time_exception();

  {
    std::lock_guard<spinlock> lock(ttable::global_lock);

    auto &ent = ttable::global[board.hash];

    if (ent.hash == board.hash) {
      if (ent.depth >= depth) {
        switch (ent.type) {
        case tt_entry_exact:
          if (best)
            *best = ent.move;
          return ent.value >= +INT_MAX - 256 ? ent.value - board.appended_moves
                 : ent.value <= -INT_MAX + 256
                     ? ent.value + board.appended_moves
                     : ent.value;
        case tt_entry_cut:
          alpha = std::max(alpha, ent.value);
          break;
        case tt_entry_all:
          beta = std::min(beta, ent.value);
          break;
        }

        if (alpha >= beta) {
          if (best)
            *best = ent.move;
          return ent.value;
        }
      }
    }
  }

  if (best)
    *best = 0;

  move bm;

  if (depth == 0)
    return quiescence(board, alpha, beta, 100, el);

  bool checked = false;
  bool check_determined = false;

  if (depth == 1) {
    check_determined = true;
    checked = board.in_check(board.side_to_move);

    if (!checked) {
      const int static_eval = evfunc(board);
      const int futility_margin = 150 + 90 * board.appended_moves;

      if (static_eval + futility_margin <= alpha)
        return static_eval;
    }
  }

  searches++;

  size_t moves_found = 0;

  // Null move pruning
  if (depth >= 3 && board.appended_moves >= 1) {
    check_determined = true;
    checked = board.in_check(board.side_to_move);

    if (!checked) {
      move _;
      board.make_move(0);
      board.appended_moves++;

      if (-perform(board, -beta, -(beta - 1), std::max(1, depth - 4), &_, el) >=
          beta) {
        board.unmake_move();
        board.appended_moves--;
        return beta;
      }

      board.unmake_move();
      board.appended_moves--;
    }
  }

  move_picker pick(board, false, true);

  int bonus = 300 - depth * 250;

  while (move m = pick.next(alpha, beta)) {
    board.make_move(m);
    board.appended_moves++;

    if (!board.in_check(board.side_to_move ^ 1)) {
      if (!moves_found && best)
        *best = m;

      branches++;

      score scr;

      int r = 1 + (depth >= 3 && moves_found >= 6 ? depth / 3 : 0);

      moves_found++;

      // PVS
      if (moves_found == 1)
        scr = -perform(board, -beta, -alpha, depth - r, &bm, el);
      else {
        scr = -perform(board, -(alpha + 1), -alpha, depth - r, &bm, el);
        if (scr > alpha && beta - alpha > 1)
          scr = -perform(board, -beta, -alpha, depth - r, &bm, el);
      }

      if (scr >= beta) {
        int ply = board.appended_moves;

        if (!CAPTURE(board.move_stack.back())) {
          // History heuristic

          int clamped_bonus =
              std::clamp(bonus, -max_history_val, max_history_val);
          int history_piece = PTYPE(board.pieces[MOVE_TO(m)]);
          int sq_to = MOVE_TO(m);

          board.history_table[history_piece][sq_to] +=
              clamped_bonus - board.history_table[history_piece][sq_to] *
                                  std::abs(clamped_bonus) / max_history_val;

          // Add killer move

          if (ply < board::max_killer_plies) {
            auto arr = board.killer_moves[ply];

            if (arr[0] != m && arr[1] != m) {
              arr[1] = arr[0];
              arr[0] = m;
            }
          }
        }

        board.unmake_move();
        board.appended_moves--;

        if (best)
          *best = m;
        alpha = scr;

        break;
      }

      if (scr > alpha) {
        if (best)
          *best = m;
        alpha = scr;
      }
    }

    board.unmake_move();
    board.appended_moves--;
  }

  if (best && *best) {
    auto e = tt_entry{
        .type = 0,
        .depth = uint8_t(depth),
        .value = alpha,
        .move = *best,
        .hash = board.hash,
    };

    if (alpha <= org_alpha)
      e.type = tt_entry_all;
    else if (alpha >= beta)
      e.type = tt_entry_cut;
    else
      e.type = tt_entry_exact;

    std::lock_guard<spinlock> guard(ttable::global_lock);

    auto &ent = ttable::global[board.hash];
    if (ent.hash != board.hash || ent.depth <= depth)
      ent = e;
  }

  if (!moves_found) {
    if (!check_determined)
      checked = board.in_check(board.side_to_move);

    return checked ? -INT_MAX + board.appended_moves : 0;
  }

  return alpha;
}

#include <omp.h>

std::chrono::high_resolution_clock::time_point nps_start, nps_end;
size_t nodes_start, nodes_end;

void search::reset_nps() {
  nodes = 0;
  nps_start = std::chrono::high_resolution_clock::now();
}

double search::nps() {
  auto cur_time = std::chrono::high_resolution_clock::now();
  auto cur_nodes = search::nodes.load();
  auto ms =
      std::chrono::duration<double, std::milli>(cur_time - nps_start).count() +
      .1;
  return 1000 * cur_nodes / ms;
}

score search::go(board &brd, move *best, int max_depth, int max_threads,
                 const execution_limiter &el, std::ostream &uci_out,
                 std::ostream &log_out) {
  reset_nps();

  score result = -INT_MAX;
  int best_depth = 0;

  // Currently can't get any speed out of Lazy SMP
  // so might as well turn this off
  max_threads = 1;

  for (int d = 2; d <= max_depth; d++) {
    std::vector<move> bests(max_threads);
    std::vector<score> results(max_threads);
    std::vector<int> done(max_threads);
    std::vector<int> depths(max_threads);

    for (int c = 0; c < max_threads; c++) {
      depths[c] = d + ctz64(c + 1) - 1;
    }

    std::vector<std::thread> threads(max_threads);

    for (int c = 0; c < max_threads; c++) {
      threads[c] = std::thread(
          [&bests, &depths, &el, &brd, &results, &done, c]() mutable {
            try {
              board b = brd;
              results[c] = search::perform(b, -INT_MAX, INT_MAX, depths[c],
                                           bests.data() + c, el);
              done[c] = 1;
            } catch (out_of_time_exception &ex) {
            }
          });
    }

    for (int c = 0; c < max_threads; c++)
      threads[c].join();

    for (int c = 0; c < max_threads; c++) {
      if (done[c] && depths[c] > best_depth) {
        result = results[c];
        *best = bests[c];
        best_depth = depths[c];
      }
    }

    std::stringstream info_line;

    info_line << format("info score cp {} depth {} nodes {} nps {}", result,
                        best_depth, nodes.load(), (uint64_t)nps());

    board copy = brd;

    constexpr int max_pv_depth = 8;

    // Print PV
    for (int i = 0; i < max_pv_depth; i++) {
      std::lock_guard<spinlock> lock(ttable::global_lock);

      auto &ent = ttable::global[copy.hash];

      if (ent.hash == copy.hash) {
        if (i == 0)
          info_line << " pv";

        move m = ent.move;
        info_line << ' ' << move_to_algebraic(m);
        copy.make_move(m);
      } else
        break;
    }

    info_line << '\n';

    uci_out << info_line.str();
    log_out << info_line.str();

    if (el.stop.load())
      break;
    if (best_depth >= max_depth)
      break;
  }

  return result;
}