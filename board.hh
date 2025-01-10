#pragma once

#include "lut.hh"
#include "util.hh"
#include <cstdint>
#include <format>
#include <iostream>
#include <string>
#include <vector>

typedef uint32_t move;
typedef uint8_t piece_state;

#define MOVE_FROM(m) ((m) & 0b111111)
#define MOVE_TO(m) (((m) >> 6) & 0b111111)
#define PROMOTION(m) (((m) >> 12) & 0b111)
#define CAPTURE(m) (((m) >> 21) & 15)
#define SET_CAPTURE(m) (move(m) << 21)
#define CAPTURE_SQUARE(m) (((m) >> 15) & 0b111111)
#define SET_CAPTURE_SQUARE(m) (move(m) << 15)
#define SIDEOF(p) ((p) & 0b1000)
#define PTYPE(p) ((p) & 0b0111)
#define S_AND_T(p) ((p) & 0b1111)
#define Y_COORD(sq) (((sq) & 0b111000) >> 3)
#define X_COORD(sq) ((sq) & 0b000111)
#define Y_ONLY(sq) ((sq) & 0b111000)
#define X_ONLY(sq) ((sq) & 0b000111)
#define SET_PAWN_LEAP(m) (move(m) << 15)
#define PAWN_LEAP(m) (((m) >> 15) & 0b111111)
#define CASTLING_BASE_BIT 28
#define KING_FILE 4
#define QUEEN_FILE 3

inline std::string move_to_algebraic(move m) {
  static const char *c[] = {"_", "_", "_", "n", "b", "r", "q", "_"};

  return std::format(
      "{}{}{}{}{}", char('a' + X_COORD(MOVE_FROM(m))),
      char('8' - Y_COORD(MOVE_FROM(m))), char('a' + X_COORD(MOVE_TO(m))),
      char('8' - Y_COORD(MOVE_TO(m))), PROMOTION(m) ? c[PROMOTION(m)] : "");
}

struct board {
  struct perft_state {
    uint64_t nodes{}, checks{}, captures{}, castles{}, promotions{}, eval{};
    FORCEINLINE perft_state &operator+=(const perft_state &r) {
      nodes += r.nodes;
      checks += r.checks;
      captures += r.captures;
      castles += r.castles;
      promotions += r.promotions;
      eval += r.eval;
      return *this;
    }
  };

  board();

  bits piece_sets[unique_pieces_count]{}, side_sets[2]{};
  bool castling_rights_lost[4]{};
  piece_state pieces[64]{};
  int appended_moves{}, side_to_move{};
  bits hash{};

  enum { max_killer_plies = 32, max_killer_moves = 2 };
  move killer_moves[max_killer_plies][max_killer_moves]{};

  uint32_t history_table[unique_pieces_count][8 * 8]{};

  std::vector<move> move_stack;

  bool import_fen(const std::string &str);
  bool import_fen(std::istream &stream);
  void make_move(move m, bool no_checks = false);
  void unmake_move(bool no_checks = false);
  void print_state(std::ostream &out) const;
  void starting_position();
  void debug_console(std::istream &in, std::ostream &out);

  inline void panic_printout() const {
    logger::err << "--- POSITION RECONSTRUCTED FROM MOVES ---\n";
    board bd;
    bd.starting_position();

    bd.print_state(logger::err);

    for (move m : move_stack) {
      logger::err << std::format("Making move: {}\n", move_to_algebraic(m));
      bd.make_move(m, true);
      bd.print_state(logger::err);
    }

    board bd2 = *this;

    logger::err << "--- ACTUAL POSITION TIMELINE ---\n";

    bd2.print_state(logger::err);

    while (bd2.move_stack.size()) {
      logger::err << std::format("Unmaking move: {}\n",
                              move_to_algebraic(bd2.move_stack.back()));
      bd2.unmake_move(true);
      bd2.print_state(logger::err);
    }
  }

  FORCEINLINE bool is_square_attacked(int side, int sq) const {
    if (sq == -1) {
      panic_printout();
      __builtin_debugtrap();
    }

    bits all_pieces = side_sets[0] | side_sets[1];

    for (int i = 0; i < lut::num_attackers; i++) {
      bits queried = side_sets[!side] & piece_sets[lut::lva[i]];

      switch (i) {
      case 4:
      case 2:
        if (lut::generate_bishop_attacks(all_pieces, sq) & queried)
          return true;
        if (i == 2)
          break;
        [[fallthrough]];
      case 3:
        if (lut::generate_rook_attacks(all_pieces, sq) & queried)
          return true;
        break;
      default:
        if (lut::attack_masks[sq][lut::lva[i]][side] & queried)
          return true;
      }
    }

    return false;
  }

  FORCEINLINE bool in_check(int side) const {
    return is_square_attacked(side, ctz64(side_sets[side] & piece_sets[king]));
  }

private:
  perft_state perft_internal(int depth);
  board::perft_state perft_backend(int depth, bool divide, std::ostream &out);
  void perft(int depth, bool divide, std::ostream &out);
  void move_piece_hash(int sqfrom, int sqto, uint8_t piece);
  void toggle_piece_hash(int sq, uint8_t piece);
  void toggle_piece_hash(int sq, uint8_t piece1, uint8_t piece2);
  void create_piece(int sq, piece_state piece);
  void create_piece_pair(int sq, piece_state piece_type, bool v, bool h);
  void change_castling_rights(move &m, int index, bool lost);
  bool is_rook_eligible_for_castling(int sq);
  void flip_castling_rights(int index);
  void flip_side();
};

inline int parse_algebraic_from_stream(std::istream &stream) {
  char x, y;
  stream >> x;
  x -= 'a';
  if (x < 0 || x > 7)
    return -1;
  stream >> y;
  y = '8' - y;
  if (y < 0 || y > 7)
    return -1;
  return x + y * 8;
}

inline move parse_move_from_stream(std::istream &stream) {
  move result = 0;
  int from = parse_algebraic_from_stream(stream);
  int to = parse_algebraic_from_stream(stream);

  if (from == -1 || to == -1)
    return 0;

  result = from | to << 6;

  switch (stream.peek()) {
  case 'n':
    result |= knight << 12;
    stream.get();
    break;
  case 'b':
    result |= bishop << 12;
    stream.get();
    break;
  case 'r':
    result |= rook << 12;
    stream.get();
    break;
  case 'q':
    result |= queen << 12;
    stream.get();
    break;
  default:
    break;
  }

  return result;
}
