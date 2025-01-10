#include "eval.hh"

#define A_FILE bits(0x0101'0101'0101'0101ull)

constexpr int MaxPhase = 16;

#define PST_BONUS(piece_name, phase, sq)                                       \
  ((mg_##piece_name##_pst[sq] * (MaxPhase - (phase)) +                         \
    eg_##piece_name##_pst[sq] * (phase)) /                                     \
   MaxPhase)

template <int side> FORCEINLINE int pawns(const board &board, int phase) {
  static const int mg_pawn_pst[64] = {
      0,   0,  0,   0,   0,   0,  0,  0,   98,  134, 61, 95,  68, 126, 34, -11,
      -6,  7,  26,  31,  65,  56, 25, -20, -14, 13,  6,  21,  23, 12,  17, -23,
      -27, -2, -5,  12,  17,  6,  10, -25, -26, -4,  -4, -10, 3,  3,   33, -12,
      -35, -1, -20, -23, -15, 24, 38, -22, 0,   0,   0,  0,   0,  0,   0,  0,
  };

  static const int eg_pawn_pst[64] = {
      0,  0,   0,  0,  0,  0,  0,  0,  178, 173, 158, 134, 147, 132, 165, 187,
      94, 100, 85, 67, 56, 53, 82, 84, 32,  24,  13,  5,   -2,  4,   17,  17,
      13, 9,   -3, -7, -7, -8, 3,  -1, 4,   7,   -6,  1,   0,   -5,  -1,  -8,
      13, 8,   8,  10, 13, 0,  2,  -7, 0,   0,   0,   0,   0,   0,   0,   0,
  };

  // count number of files with doubled pawns (10 centipawns penalty for each)
  auto all_pieces = board.piece_sets[0] | board.side_sets[1];
  auto mask = board.piece_sets[pawn] & board.side_sets[side];
  int result = POPCNT64(mask) * 100;
  auto doubles = 0;
  doubles += POPCNT64(mask & A_FILE << 0) > 1;
  doubles += POPCNT64(mask & A_FILE << 1) > 1;
  doubles += POPCNT64(mask & A_FILE << 2) > 1;
  doubles += POPCNT64(mask & A_FILE << 3) > 1;
  doubles += POPCNT64(mask & A_FILE << 4) > 1;
  doubles += POPCNT64(mask & A_FILE << 5) > 1;
  doubles += POPCNT64(mask & A_FILE << 6) > 1;
  doubles += POPCNT64(mask & A_FILE << 7) > 1;
  result -= doubles * 10;

  // count number of blocked pawns (10 centipawns penalty for each)
  auto bmask = mask;
  if (side)
    bmask >>= 8;
  else
    bmask <<= 8;
  auto blocks = POPCNT64(bmask & all_pieces);
  result -= blocks * 10;

  uint32_t flip = !side * 0b111000;

  while (mask) {
    int sq = ctz64(mask) ^ flip;
    result += PST_BONUS(pawn, phase, sq);
    POP_LSB(mask);
  }

  return result;
}

template <int side> FORCEINLINE int knights(const board &board) {
  auto mask = board.piece_sets[knight] & board.side_sets[side];
  int result = POPCNT64(mask) * 413;

  // mobility
  while (mask) {
    result += POPCNT64((lut::attack_masks[ctz64(mask)][knight][0]) &
                       ~board.side_sets[side]) *
              3;
    POP_LSB(mask);
  }

  return result;
}

template <int side> FORCEINLINE int bishops(const board &board) {
  auto all_pieces = board.side_sets[0] | board.side_sets[1];
  auto mask = board.piece_sets[bishop] & board.side_sets[side];
  int result = POPCNT64(mask) * 422;

  // mobility
  while (mask) {
    result += POPCNT64(lut::generate_bishop_attacks(all_pieces, ctz64(mask)) &
                       ~board.side_sets[side]) *
              3;
    POP_LSB(mask);
  }

  return result;
}

template <int side> FORCEINLINE int rooks(const board &board) {
  auto mask = board.piece_sets[rook] & board.side_sets[side];
  return POPCNT64(mask) * 633;
}

template <int side> FORCEINLINE int queens(const board &board, int phase) {
  static const int mg_queen_pst[64] = {
      -28, 0,   29, 12,  59, 44, 43, 45, -24, -39, -5,  1,   -16, 57,  28,  54,
      -13, -17, 7,  8,   29, 56, 47, 57, -27, -27, -16, -16, -1,  17,  -2,  1,
      -9,  -26, -9, -10, -2, -4, 3,  -3, -14, 2,   -11, -2,  -5,  2,   14,  5,
      -35, -8,  11, 2,   8,  15, -3, 1,  -1,  -18, -9,  10,  -15, -25, -31, -50,
  };

  static const int eg_queen_pst[64] = {
      -9,  22,  22,  27,  27,  19,  10,  20,  -17, 20,  32,  41,  58,
      25,  30,  0,   -20, 6,   9,   49,  47,  35,  19,  9,   3,   22,
      24,  45,  57,  40,  57,  36,  -18, 28,  19,  47,  31,  34,  39,
      23,  -16, -27, 15,  6,   9,   17,  10,  5,   -22, -23, -30, -16,
      -16, -23, -36, -32, -33, -28, -22, -43, -5,  -32, -20, -41,
  };

  auto mask = board.piece_sets[queen] & board.side_sets[side];
  int result = POPCNT64(mask) * 1274;

  uint32_t flip = !side * 0b111000;

  while (mask) {
    int sq = ctz64(mask) ^ flip;
    result += PST_BONUS(queen, phase, sq);
    POP_LSB(mask);
  }

  return result;
}

template <int side> FORCEINLINE int kings(const board &board, int phase) {
  static const int mg_king_pst[64] = {
      -65, 23,  16,  -15, -56, -34, 2,   13,  29,  -1,  -20, -7,  -8,
      -4,  -38, -29, -9,  24,  2,   -16, -20, 6,   22,  -22, -17, -20,
      -12, -27, -30, -25, -14, -36, -49, -1,  -27, -39, -46, -44, -33,
      -51, -14, -14, -22, -46, -44, -30, -15, -27, 1,   7,   -8,  -64,
      -43, -16, 9,   8,   -15, 36,  12,  -54, 8,   -28, 24,  14,
  };

  static const int eg_king_pst[64] = {
      -74, -35, -18, -18, -11, 15,  4,   -17, -12, 17,  14,  17, 17,
      38,  23,  11,  10,  17,  23,  15,  20,  45,  44,  13,  -8, 22,
      24,  27,  26,  33,  26,  3,   -18, -4,  21,  24,  27,  23, 9,
      -11, -19, -3,  11,  21,  23,  16,  7,   -9,  -27, -11, 4,  13,
      14,  4,   -5,  -17, -53, -34, -21, -11, -28, -14, -24, -43};

  int ksq = ctz64(board.piece_sets[king] & board.side_sets[side]);

  bits kzone = lut::attack_masks[ksq][king_zone][side];

  ksq ^= 0b111000 * !side;
  int score = PST_BONUS(king, phase, ksq);

  // 40 centipawns penalty for each attackable square in king zone
  while (kzone) {
    score -= 40 * board.is_square_attacked(side, ctz64(kzone));
    POP_LSB(kzone);
  }

  return score;
}

FORCEINLINE int game_phase_score(const board &board) {
  return MaxPhase -
         std::min(MaxPhase, POPCNT64(board.piece_sets[queen]) * 4 +
                                POPCNT64(board.piece_sets[rook]) * 2 +
                                POPCNT64(board.piece_sets[bishop]) +
                                POPCNT64(board.piece_sets[knight]));
}

namespace eval {
int myeval(const board &board) {
  int gps = game_phase_score(board);

  int wpawns = pawns<1>(board, gps);
  int wknights = knights<1>(board);
  int wbishops = bishops<1>(board);
  int wrooks = rooks<1>(board);
  int wqueens = queens<1>(board, gps);
  int wkings = kings<1>(board, gps);

  int bpawns = pawns<0>(board, gps);
  int bknights = knights<0>(board);
  int bbishops = bishops<0>(board);
  int brooks = rooks<0>(board);
  int bqueens = queens<0>(board, gps);
  int bkings = kings<0>(board, gps);

  return (board.side_to_move * 2 - 1) *
         (wpawns - bpawns + wknights - bknights + wbishops - bbishops + wrooks -
          brooks + wqueens - bqueens + wkings - bkings);
}
}
