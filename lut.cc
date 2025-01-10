#include "lut.hh"
#include "board.hh"
#include "magicfactors.hh"
#include "util.hh"
#include <bit>
#include <cstring>
#include <functional>
#include <random>

namespace lut {
bits attack_masks[64][unique_pieces_count + 2][2];
bits sliding_attack_tables[1024 * 1024 / sizeof(bits)] = {};

gmagic bishop_magic[64], rook_magic[64];

const int mvv[] = {queen, rook, bishop, knight, pawn};
const int lva[] = {pawn, knight, bishop, rook, queen, king};

bits occupancy_to_bishop_attack(bits b, int i, int j) {
  bits output = 0;

  int I, J;

#define occupancy_to_attack(op1, op2)                                          \
  for (I = i, J = j, op1 I, op2 J; I >= 0 && J >= 0 && I < 8 && J < 8;         \
       op1 I, op2 J) {                                                         \
    bits mask = bits(1) << (J * 8 + I);                                        \
    output |= mask;                                                            \
    if (b & mask)                                                              \
      break;                                                                   \
  }

  occupancy_to_attack(++, ++) occupancy_to_attack(++, --)
      occupancy_to_attack(--, ++) occupancy_to_attack(--, --)

          return output;
}

bits occupancy_to_rook_attack(bits b, int i, int j) {
  bits output = 0;

  int I, J;

  occupancy_to_attack(++, ) occupancy_to_attack(--, ) occupancy_to_attack(, ++)
      occupancy_to_attack(, --)

#undef occupancy_to_attack

          return output;
}

bits random_seed = 333;

void print_bitboard(bits b) {
  for (int y = 0; y < 8; y++) {
    for (int x = 0; x < 8; x++)
      std::putchar('0' + (b >> (x + y * 8) & 1));
    std::putchar('\n');
  }
}

void print_magic_factors() {
  std::printf("bits bishop_factors[64] = {\n");
  for (int i = 0; i < 64; i++)
    std::printf("\t0x%llx,\n", (long long)bishop_magic[i].magic);
  std::printf("};\n\n");

  std::printf("bits rook_factors[64] = {\n");
  for (int i = 0; i < 64; i++)
    std::printf("\t0x%llx,\n", (long long)rook_magic[i].magic);
  std::printf("};\n\n");
}

void test_magic_factors() {
  std::mt19937_64 e(random_seed);

  auto func1 = occupancy_to_rook_attack;
  auto func2 = generate_rook_attacks;

  for (;;) {
    int i = e() & 7, j = e() & 7;
    bits occ = e();
    bits real = func1(occ, i, j);
    bits magic = func2(occ, i + j * 8);

    if (real != magic) {
      std::printf("Mismatch detected @ (%i; %i)\n", i, j);
      std::printf("occ = \n");
      print_bitboard(occ);
      std::printf("real = \n");
      print_bitboard(real);
      std::printf("magic = \n");
      print_bitboard(magic);
      break;
    }
  }
}

void generate_data() {
  std::mt19937_64 e(random_seed);

  bits *current_ptr = sliding_attack_tables;

  for (int side = 0; side < 2; side++)
    for (int y = 0, I = 0; y < 8; y++) {
      for (int x = 0; x < 8; x++, I++) {
        auto a = [x, y](int i, int j) mutable {
          i += x, j += y;
          if ((i & 7) == i && (j & 7) == j)
            return 1ull << (i + j * 8);
          return 0ull;
        };

        auto b = [x, y](int i, int j, int block_x = 1,
                        int block_y = 1) mutable {
          i += x, j += y;
          if ((!block_x || (i >= 1 && i <= 6)) && (!block_y || (j >= 1 && j <= 6)))
            return 1ull << (i + j * 8);
          return 0ull;
        };

        for (int i = -2; i <= 2; i += 4)
          for (int j = -1; j <= 1; j += 2) {
            attack_masks[I][knight][side] |= a(i, j);
            attack_masks[I][knight][side] |= a(j, i);
          }

        for (int j = -1; j <= 1; j += 2) {
          for (int i = -1; i <= 1; i++)
            attack_masks[I][king][side] |= a(i, j);
          attack_masks[I][king][side] |= a(j, 0);
        }

        for (int j = -1; j <= 1; j += 2)
          attack_masks[I][pawn][side] |= a(j, 1 - side * 2);

        attack_masks[I][king_zone][side] |= attack_masks[I][king][side];
        for (int j = -1; j <= 1; j++)
          attack_masks[I][king_zone][side] |= a(j, -side * 2);

        if (!side) {

          bits bishop_mask = 0, rook_mask = 0;
          bits bpm = 0, rpm = 0;

          for (int i = 1; i <= 8; i++) {
            bishop_mask |= b(i, i) | b(i, -i);
            bishop_mask |= b(-i, i) | b(-i, -i);
            rook_mask |= b(i, 0, 1, 0) | b(-i, 0, 1, 0);
            rook_mask |= b(0, i, 0, 1) | b(0, -i, 0, 1);

            bpm |= a(i, i) | a(i, -i);
            bpm |= a(-i, i) | a(-i, -i);
            rpm |= a(i, 0) | a(-i, 0);
            rpm |= a(0, i) | a(0, -i);
          }

          bishop_magic[I].shift = 64 - int(POPCNT64(bishop_mask));
          bishop_magic[I].mask = bishop_mask;
          rook_magic[I].shift = 64 - int(POPCNT64(rook_mask));
          rook_magic[I].mask = rook_mask;

          auto fill_with_magic =
              [&](const char *doc, bits candidate, gmagic &mag, bits postmask,
                  std::function<bits(bits, int, int)> convert) mutable -> bits {
            mag.ptr = current_ptr;

            for (;;) {
              std::memset(mag.ptr, 0xff,
                          size_t(sizeof(bits)) << (64 - mag.shift));

              bits sub = 0, max_idx = 0;
              bool collision = false;

              do {
                bits idx = sub & mag.mask;
                idx *= candidate;
                idx >>= mag.shift;
                bits &el = mag.ptr[idx];
                bits needed_value = convert(sub, x, y);

                max_idx = std::max(max_idx, idx);

                if (el != ~bits(0) && el != needed_value) {
                  collision = true;
                  break;
                } else
                  el = needed_value;
              } while ((sub = (sub - postmask) & postmask));

              if (!collision) {
                current_ptr += max_idx + 1;
                return mag.magic = candidate;
              }

              bits e1 = e(), e2 = e(), e3 = e();
              candidate = e1 & e2 & e3;
            }

            return 0;
          };

          fill_with_magic("bishop", bishop_factors[I], bishop_magic[I], bpm,
                          occupancy_to_bishop_attack);
          fill_with_magic("rook", rook_factors[I], rook_magic[I], rpm,
                          occupancy_to_rook_attack);
        }
      }
    }
}
} // namespace lut