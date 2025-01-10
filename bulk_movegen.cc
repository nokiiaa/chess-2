#include "bulk_movegen.hh"
#include "board.hh"
#include "lut.hh"
#include <cassert>

bulk_move_generator::bulk_move_generator(const board &b) : _board(b) {}

void legality_guard(const board &bd, move m) {
  /*if (!bulk_move_generator::is_move_pseudolegal((bd), (m))) {
    logger::err << "Offender: " << move_to_algebraic(m) << '\n';
    bd.print_state(logger::err);
    assert(0);
  }*/
}

#define GEN_MOVE(ptr, sq_from, sq_to)                                          \
  do {                                                                         \
    move _m = (*(ptr++) = (sq_from) | (sq_to) << 6);                           \
    legality_guard(_board, _m);                                                \
  } while (0)

#define GEN_PROMOTION(ptr, sq_from, sq_to)                                     \
  *(ptr)++ = (sq_from) | (sq_to) << 6 | queen << 12;                           \
  *(ptr)++ = (sq_from) | (sq_to) << 6 | knight << 12;                          \
  *(ptr)++ = (sq_from) | (sq_to) << 6 | bishop << 12;                          \
  *(ptr)++ = (sq_from) | (sq_to) << 6 | rook << 12;                            \
  legality_guard(_board, (sq_from) | (sq_to) << 6 | rook << 12);

#define GEN_PROMOTIONS_FROM_MASK(ptr, sq_from, mask)                           \
  do {                                                                         \
    bits _m = (mask);                                                          \
    while (_m) {                                                               \
      auto b = ctz64(_m);                                                      \
      GEN_PROMOTION(ptr, sq_from, b)                                           \
      POP_LSB(_m);                                                             \
    }                                                                          \
  } while (0)

#define GEN_MOVES_FROM_MASK(ptr, sq_from, mask)                                \
  do {                                                                         \
    bits _m = (mask);                                                          \
    while (_m) {                                                               \
      auto b = ctz64(_m);                                                      \
      GEN_MOVE(ptr, sq_from, b);                                               \
      POP_LSB(_m);                                                             \
    }                                                                          \
  } while (0)

move *bulk_move_generator::gen_promotions_captures(move *ptr) {
  bits us = _board.side_sets[_board.side_to_move];
  bits them = _board.side_sets[_board.side_to_move ^ 1];
  bits all = us | them;

  bits p = us & _board.piece_sets[pawn];

  bits anti_blockers_mask =
      (~all & bits(0xff00'0000'0000'0000ull) >> (56 * _board.side_to_move));

  if (_board.side_to_move)
    anti_blockers_mask <<= 8;
  else
    anti_blockers_mask >>= 8;

  // candidates for non-capture promotion
  p = _board.side_sets[_board.side_to_move] & _board.piece_sets[pawn] &
      bits(0x00ff'0000'0000'0000ull) >> (40 * _board.side_to_move) &
      anti_blockers_mask;

  // non-capture promotions
  while (p) {
    int sq = ctz64(p);
    int to = sq + 8 - _board.side_to_move * 16;
    GEN_PROMOTION(ptr, sq, to);
    POP_LSB(p);
  }

  p = _board.piece_sets[pawn] & us;

  bits en_passant_bit = 0;

  if (_board.move_stack.size()) {
    move lm = _board.move_stack.back();
    int sq = PAWN_LEAP(lm);
    if (!CAPTURE(lm) && sq) {
      // XOR'ing by 0b001000
      // transforms Y-coordinate 4 -> 5 and 3 -> 2
      // (just what we need for reducing a pawn leap one square)
      sq ^= 0b001000;
      en_passant_bit = bits(1) << sq;
    }
  }

  while (p) {
    int sq = ctz64(p);
    auto m = lut::attack_masks[sq][pawn][_board.side_to_move] &
             (them | en_passant_bit);
    if (m & bits(0xff00'0000'0000'00ffull))
      GEN_PROMOTIONS_FROM_MASK(ptr, sq, m);
    else
      GEN_MOVES_FROM_MASK(ptr, sq, m);
    POP_LSB(p);
  }

#define NON_SLIDING(type, target_mask)                                         \
  p = us & _board.piece_sets[type];                                            \
  while (p) {                                                                  \
    int sq = ctz64(p);                                                         \
    auto m = lut::attack_masks[sq][type][_board.side_to_move] & target_mask;   \
    GEN_MOVES_FROM_MASK(ptr, sq, m);                                           \
    POP_LSB(p);                                                                \
  }

  NON_SLIDING(knight, them)
  NON_SLIDING(king, them)

#define SLIDING(type, is_rook, is_bishop, target_mask)                         \
  p = us & _board.piece_sets[type];                                            \
  while (p) {                                                                  \
    int sq = ctz64(p);                                                         \
    bits m = 0;                                                                \
    if (is_bishop)                                                             \
      m |= lut::generate_bishop_attacks(all, sq) & target_mask;                \
    if (is_rook)                                                               \
      m |= lut::generate_rook_attacks(all, sq) & target_mask;                  \
    GEN_MOVES_FROM_MASK(ptr, sq, m);                                           \
    POP_LSB(p);                                                                \
  }

  SLIDING(bishop, false, true, them)
  SLIDING(rook, true, false, them)
  SLIDING(queen, true, true, them)

  return ptr;
}

// clang-format off

static const bits pawn_shift_masks[2] =
{
  0x00'00'00'00'01'01'00'00ull,
  0x00'00'01'01'00'00'00'00ull,
},
king_castling_occ_masks[4] = {
  bits(0b0000'1110) << 0,  // black queenside
  bits(0b0110'0000) << 0,  // black kingside
  bits(0b0000'1110) << 56, // white queenside
  bits(0b0110'0000) << 56, // white kingside
};

// clang-format on

#define CASTLING_CONDITIONS(_board, ksq, op)                                   \
  (!(all & king_castling_occ_masks[castling_idx]) &&                           \
   !_board.castling_rights_lost[castling_idx] &&                               \
   !_board.is_square_attacked(_board.side_to_move, (ksq)) &&                   \
   !_board.is_square_attacked(_board.side_to_move, (ksq)op 1) &&               \
   !_board.is_square_attacked(_board.side_to_move, (ksq)op 2))

move *bulk_move_generator::gen_noncaptures(move *ptr) {
  int stm = _board.side_to_move;
  bits us = _board.side_sets[stm];
  bits them = _board.side_sets[stm ^ 1];
  bits all = us | them;
  bits free = ~all;

  bits p = _board.piece_sets[pawn] & us;

  // Pawn moves
  while (p) {
    int sq = ctz64(p);

    bits m = 0;

    if (Y_COORD(sq) == 1 + stm * 5)
      // If no obstruction:
      if (!(all & pawn_shift_masks[stm] << X_ONLY(sq)))
        m |= bits(1) << (sq + 16 - stm * 32);

    // Normal pawn move, one square up/down (not a promotion)
    if (Y_COORD(sq) != 6 - 5 * stm) {
      m |= bits(1) << (sq + 8 - stm * 16) & free;
    }

    GEN_MOVES_FROM_MASK(ptr, sq, m);

    POP_LSB(p);
  }

  auto ksq = ctz64(_board.piece_sets[king] & us);
  // Attempt to generate queenside castle
  int castling_idx = stm * 2 + 0;
  if (CASTLING_CONDITIONS(_board, ksq, -))
    GEN_MOVE(ptr, ksq, ksq - 2);
  // Attempt to generate kingside castle
  castling_idx++;
  if (CASTLING_CONDITIONS(_board, ksq, +))
    GEN_MOVE(ptr, ksq, ksq + 2);

  NON_SLIDING(knight, free)
  NON_SLIDING(king, free)

  SLIDING(bishop, false, true, free)
  SLIDING(rook, true, false, free)
  SLIDING(queen, true, true, free)

  return ptr;
}

bool bulk_move_generator::is_move_pseudolegal(const board &b, move m) {
  // return false;
  bits us = b.side_sets[b.side_to_move];
  bits them = b.side_sets[b.side_to_move ^ 1];
  bits all = us | them;

  int from_sq = MOVE_FROM(m);
  int to_sq = MOVE_TO(m);
  bits to_sq_mask = bits(1) << MOVE_TO(m);
  bits to_mask = ~us & to_sq_mask;

  if (!to_mask)
    return false;

#define SLIDING_TEST(is_rook, is_bishop)                                       \
  if (is_bishop && (lut::generate_bishop_attacks(all, from_sq) & to_mask))     \
    return true;                                                               \
  if (is_rook && (lut::generate_rook_attacks(all, from_sq) & to_mask))         \
    return true;

#define NON_SLIDING_TEST(type)                                                 \
  if (lut::attack_masks[from_sq][type][b.side_to_move] & to_mask)              \
    return true;

#define NON_SLIDING_CAPTURE_TEST(type)                                         \
  if (lut::attack_masks[from_sq][type][b.side_to_move] & to_mask & them)       \
    return true;

  if (!b.pieces[from_sq] || !!SIDEOF(b.pieces[from_sq]) != b.side_to_move)
    return false;

  int type = PTYPE(b.pieces[from_sq]);

  if ((PROMOTION(m) && type != pawn) || (m >> 15))
    return false;

  switch (type) {
  case bishop:
    SLIDING_TEST(false, true) break;
  case rook:
    SLIDING_TEST(true, false) break;
  case queen:
    SLIDING_TEST(true, true) break;
  case knight:
    NON_SLIDING_TEST(knight) break;
  case king: {
    NON_SLIDING_TEST(king)

    // Attempt to generate queenside castle
    int castling_idx = b.side_to_move * 2 + 0;
    if (to_sq == from_sq - 2 && CASTLING_CONDITIONS(b, from_sq, -))
      return true;

    // Attempt to generate kingside castle
    castling_idx++;
    if (to_sq == from_sq + 2 && CASTLING_CONDITIONS(b, from_sq, +))
      return true;
    break;
  }
  case pawn: {
    switch (PROMOTION(m)) {
    case 0:
      // Promotion cannot be null if it's moving to the top rank
      if (Y_COORD(to_sq) == (1 - b.side_to_move) * 7)
        return false;
      break;
    case rook:
    case knight:
    case bishop:
    case queen:
      // Promotion cannot be non-null if it's not moving to the top rank
      if (Y_COORD(to_sq) != (1 - b.side_to_move) * 7)
        return false;
      break;
    default:
      // invalid promotion
      return false;
    }

    if (X_COORD(from_sq) == X_COORD(to_sq)) {
      int side_factor = 8 * (1 - b.side_to_move * 2);

      if (to_sq == from_sq + side_factor && !(all & to_sq_mask))
        return true;
      else if (
          // dest_y=3 for black pawn leap,
          // dest_y=4 for white pawn leap
          Y_COORD(to_sq) == 3 + b.side_to_move &&
          to_sq == from_sq + side_factor * 2 && !(all & to_sq_mask) &&
          !(all & bits(1) << (to_sq - side_factor)))
        return true;
    }

    // pawn captures
    NON_SLIDING_CAPTURE_TEST(pawn)

    bits en_passant_bit = 0;

    if (b.move_stack.size()) {
      move lm = b.move_stack.back();
      int sq = PAWN_LEAP(lm);
      if (!CAPTURE(lm) && sq) {
        sq ^= 0b001000;

        en_passant_bit = bits(1) << sq;
      }
    }

    if (lut::attack_masks[from_sq][pawn][b.side_to_move] & en_passant_bit & to_mask)
      return true;

    break;
  }
  }

  return false;
}