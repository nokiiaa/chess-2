#include "seq_movegen.hh"
#include "lut.hh"
#include "util.hh"
#include <mutex>

move_generator::move_generator(const board &board) : _board(board) {}

void move_generator::start_captures() {
  _captures_of = 0;
  _captor = 0;
  _captures_of_bitmask = 0;
  _captors_bitmask = 0;
  _en_passant_bit = 0;
  _en_passant_sq = -1;
  _all_pieces = _board.side_sets[0] | _board.side_sets[1];

  for (int i = 0; i < lut::num_victims; i++)
    _victims[i] =
        _board.piece_sets[lut::mvv[i]] & _board.side_sets[!_board.side_to_move];

  if (_board.move_stack.size()) {
    move lm = _board.move_stack.back();
    int sq = PAWN_LEAP(lm);
    if (!CAPTURE(lm) && sq) {
      // XOR'ing by 0b001000
      // transforms Y-coordinate 4 -> 5 and 3 -> 2
      // (just what we need for reducing a pawn leap one square)
      sq ^= 0b001000;
      _en_passant_bit = bits(1) << sq;
      _en_passant_sq = sq;
      _victims[0] |= _en_passant_bit;
    }
  }

  for (int i = 0; i < lut::num_attackers; i++)
    _attackers[i] =
        _board.piece_sets[lut::lva[i]] & _board.side_sets[_board.side_to_move];
}

void move_generator::start_noncaptures() {
  _friendly_pieces_mask = _board.side_sets[_board.side_to_move];
  _free_squares_mask = 0;
  _all_pieces = _board.side_sets[0] | _board.side_sets[1];
  _src_square = 0;
}

void move_generator::start_promotions() {
  _all_pieces = _board.side_sets[0] | _board.side_sets[1];

  bits anti_blockers_mask = (~_all_pieces & bits(0xff00'0000'0000'0000ull) >>
                                                (56 * _board.side_to_move));

  if (_board.side_to_move)
    anti_blockers_mask <<= 8;
  else
    anti_blockers_mask >>= 8;

  _candidate_pawns = _board.side_sets[_board.side_to_move] &
                     _board.piece_sets[pawn] &
                     bits(0x00ff'0000'0000'0000) >> (40 * _board.side_to_move) &
                     anti_blockers_mask;
}

move move_generator::next_capture() {
  int stm = _board.side_to_move;
  int dst_square;

  for (;;) {
    while (!_captures_of_bitmask && _captures_of < lut::num_victims)
      _captures_of_bitmask = _victims[_captures_of++];

    if (!_captures_of_bitmask)
      return 0;

    dst_square = ctz64(_captures_of_bitmask);

    for (; !_captors_bitmask && _captor < lut::num_attackers; _captor++) {
      if ((bits)dst_square == _en_passant_sq && _captor != 0)
        continue;

      bits queried = _attackers[_captor];

      switch (_captor) {
      case 4:
      case 2:
        _captors_bitmask |=
            lut::generate_bishop_attacks(_all_pieces, dst_square) & queried;
        if (_captor == 2)
          break;
        [[fallthrough]];
      case 3:
        _captors_bitmask |=
            lut::generate_rook_attacks(_all_pieces, dst_square) & queried;
        break;
      default:
        // pawn, knight, king
        _captors_bitmask =
            lut::attack_masks[dst_square][lut::lva[_captor]][!stm] & queried;
        break;
      }
    }

    if (_captors_bitmask) {
      int src_square = ctz64(_captors_bitmask);
      POP_LSB(_captors_bitmask);
      return move(src_square | dst_square << 6);
    } else {
      POP_LSB(_captures_of_bitmask);
      _captor = 0;
    }
  }
}

move move_generator::next_noncapture() {
  // clang-format off
  static const bits pawn_shift_masks[2] = {
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

  int stm = _board.side_to_move;
  int dst_square;

  while (!_free_squares_mask && _friendly_pieces_mask) {
    _src_square = ctz64(_friendly_pieces_mask);

    int type = PTYPE(_board.pieces[_src_square]);
    int castling_idx;
    switch (type) {
    case pawn:
      // Pawn leap
      if (Y_COORD(_src_square) == 1 + stm * 5)
        // If no obstruction:
        if (!(_all_pieces & pawn_shift_masks[stm] << X_ONLY(_src_square)))
          _free_squares_mask |= bits(1) << (_src_square + 16 - stm * 32);
      // Normal pawn move, one square up/down (not a promotion)
      if (Y_COORD(_src_square) != 6 - 5 * stm)
        _free_squares_mask |=
            bits(1) << (_src_square + 8 - stm * 16) & ~_all_pieces;
      break;
    case king:
      // Attempt to generate queenside castle
      castling_idx = stm * 2 + 0;
      if ( // no pieces between king and rook
          !(_all_pieces & king_castling_occ_masks[castling_idx]) &&
          // castling rights not lost
          !_board.castling_rights_lost[castling_idx] &&
          // not currently in check
          !_board.is_square_attacked(stm, _src_square - 0) &&
          // not castling through check
          !_board.is_square_attacked(stm, _src_square - 1) &&
          // not castling into check
          !_board.is_square_attacked(stm, _src_square - 2))
        _free_squares_mask |= bits(1) << (_src_square - 2);
      // Attempt to generate kingside castle
      castling_idx++;
      if (!(_all_pieces & king_castling_occ_masks[castling_idx]) &&
          !_board.castling_rights_lost[castling_idx] &&
          !_board.is_square_attacked(stm, _src_square + 0) &&
          !_board.is_square_attacked(stm, _src_square + 1) &&
          !_board.is_square_attacked(stm, _src_square + 2))
        _free_squares_mask |= bits(1) << (_src_square + 2);
      [[fallthrough]];
    case knight:
      _free_squares_mask |=
          lut::attack_masks[_src_square][type][0] & ~_all_pieces;
      break;
    case queen:
    case bishop:
      _free_squares_mask |=
          lut::generate_bishop_attacks(_all_pieces, _src_square) & ~_all_pieces;
      if (type == bishop)
        break;
      [[fallthrough]];
    case rook:
      _free_squares_mask |=
          lut::generate_rook_attacks(_all_pieces, _src_square) & ~_all_pieces;
      break;
    }

    POP_LSB(_friendly_pieces_mask);
  }

  if (!_free_squares_mask)
    return 0;

  dst_square = ctz64(_free_squares_mask);
  POP_LSB(_free_squares_mask);

  return move(_src_square | dst_square << 6);
}

move move_generator::next_promotion() {
  if (!_candidate_pawns)
    return 0;

  int src_square = ctz64(_candidate_pawns);
  int dst_square = src_square + 8 - _board.side_to_move * 16;
  POP_LSB(_candidate_pawns);

  return move(src_square | dst_square << 6);
}

static constexpr int _num_promotion_types = 4;
static const int _promotions_array[]{queen << 12, knight << 12, rook << 12,
                                     bishop << 12};

void move_generator::start(bool no_quiet_moves) {
  _no_quiet_moves = no_quiet_moves;
  _phase = 0;
  _last_move = 0;
  _promotions_counter = _num_promotion_types;
  start_promotions();
}

FORCEINLINE bool move_generator::is_move_promotion(move m) {
  return (S_AND_T(_board.pieces[MOVE_FROM(m)]) == (pawn | black) &&
          Y_COORD(MOVE_TO(m)) == 7) ||
         (S_AND_T(_board.pieces[MOVE_FROM(m)]) == (pawn | white) &&
          Y_COORD(MOVE_TO(m)) == 0);
}

#include "tt.hh"

move move_generator::next() {
  if (_promotions_counter < _num_promotion_types)
    return _last_move | _promotions_array[_promotions_counter++];

  switch (_phase) {
  // Hash move
  case 0: {
    if (!_no_quiet_moves) {
      std::lock_guard<spinlock> lock(ttable::global_lock);

      const auto &ent = ttable::global[_board.hash];

      if (ent.hash == _board.hash) {
        _phase++;
        return ent.move;
      }
    }

    _phase++;
  }
    [[fallthrough]];
  // Generate promotions
  case 1:
    if ((_last_move = next_promotion()) != 0) {
      _promotions_counter = 1;
      return _last_move | _promotions_array[0];
    }

    start_captures();
    _phase++;
    [[fallthrough]];
  // Generate captures
  case 2:
    if ((_last_move = next_capture()) != 0) {
      if (is_move_promotion(_last_move)) {
        _promotions_counter = 1;
        return _last_move | _promotions_array[0];
      }

      return _last_move;
    }

    if (_no_quiet_moves)
      return 0;

    start_noncaptures();
    _phase++;
    [[fallthrough]];
  // Generate non-captures (except for promotions)
  case 3:
    return next_noncapture();
  default:
    return 0;
  }
}