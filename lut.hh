#pragma once

#include "util.hh"

enum {
  no_piece,
  pawn,
  king,
  knight,
  bishop,
  rook,
  queen,
  unique_pieces_count,
  king_zone,
  black = 0,
  white = 0b1000,
};

namespace lut {
extern bits attack_masks[64][unique_pieces_count + 2][2];
extern bits sliding_attack_tables[];
static constexpr int num_victims = 5, num_attackers = 6;
extern const int mvv[num_victims], lva[num_attackers];

struct gmagic {
  bits *ptr;
  bits mask, magic;
  int shift;
};

extern gmagic bishop_magic[64], rook_magic[64];

void generate_data();

FORCEINLINE bits generate_bishop_attacks(bits occ, int sq) {
  occ &= bishop_magic[sq].mask;
  occ *= bishop_magic[sq].magic;
  occ >>= bishop_magic[sq].shift;
  return bishop_magic[sq].ptr[occ];
}

FORCEINLINE bits generate_rook_attacks(bits occ, int sq) {
  occ &= rook_magic[sq].mask;
  occ *= rook_magic[sq].magic;
  occ >>= rook_magic[sq].shift;
  return rook_magic[sq].ptr[occ];
}
} // namespace lut