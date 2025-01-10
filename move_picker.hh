#pragma once
#include "bulk_movegen.hh"
#include <cstdint>
#include <random>

using score = std::int64_t;

struct move_picker {
	inline move_picker(board& b, bool tactical_only, bool sort_moves)
		: tactical_only(tactical_only), sort_moves(sort_moves), bd(b), gen(b) {}
	move next(score alpha, score beta);
private:
	static constexpr int MaxMoves = 128;
	int phase{};
	bool tactical_only, sort_moves;
	board& bd;
	bulk_move_generator gen;
	move tactical_moves[MaxMoves], quiet_moves[MaxMoves];
	int tactical_scores[MaxMoves], quiet_scores[MaxMoves];
	size_t num_tactical_moves{},
		num_quiet_moves{}, i_tactical{}, i_quiet{};
};