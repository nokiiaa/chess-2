#pragma once

#include "board.hh"
#include "lut.hh"

class bulk_move_generator {
public:
	bulk_move_generator(const board& board);
	move *gen_promotions_captures(move* ptr);
	move *gen_noncaptures(move* ptr);
	inline move *gen_all(move* ptr) {
		ptr = gen_promotions_captures(ptr);
		return gen_noncaptures(ptr);
	}
	static bool is_move_pseudolegal(const board& board, move m);
private:
	const board& _board;
};
