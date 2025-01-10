#pragma once

#include "move_picker.hh"
#include "util.hh"
#include "board.hh"

enum {
	tt_entry_exact = 1,
	tt_entry_all = 2,
	tt_entry_cut = 3
};

struct tt_entry {
	uint8_t type, depth;
	score value;
	move move;
	bits hash;
};

struct ttable {
	size_t size;
	tt_entry *entries;

	inline ttable() : size{}, entries{} {}

	inline ttable(int size_shift) {
		entries = new tt_entry[size = size_t(1) << size_shift];
	}

	FORCEINLINE tt_entry& operator[](bits hash) { return entries[hash & (size - 1)]; }

	static spinlock global_lock;
	static ttable global;

	static inline void init_global(int size_shift) {
		global = ttable(size_shift);
	}
};
