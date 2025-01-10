#pragma once

#include "board.hh"
#include "bulk_movegen.hh"
#include "eval.hh"
#include "move_picker.hh"
#include <atomic>


struct out_of_time_exception : std::exception {};

struct execution_limiter {
  std::atomic<bool> stop;
};

namespace search {

constexpr int max_history_val = 10000;

void reset_nps();

double nps();

score quiescence(board &board, score alpha, score beta, int depth,
                         const execution_limiter &el);

score perform(board &board, score alpha, score beta, int depth, move *best,
              const execution_limiter &el);

score go(board &brd, move *best, int max_depth, int max_threads,
         const execution_limiter &el, std::ostream &uci_out,
         std::ostream &log_out);

extern std::atomic<size_t> branches, searches, nodes;

inline double avg_branching_factor() { return (double)branches / searches; };

} // namespace search