#pragma once

#include "board.hh"
#include "lut.hh"

class move_generator {
public:
	move_generator(const board& board);

	void start(bool no_quiet_moves = false);
	move next();
private:
	bits _victims[lut::num_victims], _attackers[lut::num_attackers];

	const board& _board;
	int _phase{};
	bool _no_quiet_moves;
	move _last_move;
	int _captures_of, _captor;
	bits _current_victim, _captors_bitmask,
		_en_passant_bit, _en_passant_sq, _captures_of_bitmask,
		_friendly_pieces_mask, _free_squares_mask,
		_all_pieces, _src_square, _candidate_pawns,
		_promotions_counter;

	void start_captures(), start_noncaptures(), start_promotions();
	move next_capture(), next_noncapture(), next_promotion();
	bool is_move_promotion(move m);
};