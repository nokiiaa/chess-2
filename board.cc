#include "board.hh"
#include "bulk_movegen.hh"
#include "util.hh"
#include <mutex>
#include <thread>

enum { queenside = 0, kingside = 1, zobrist_stm_flag = 4 };

#define CASTLE(X)                                                              \
  if (PTYPE(X) == king) {                                                      \
    int xdiff = X_ONLY(sqto) - X_ONLY(sqfrom);                                 \
                                                                               \
    if (xdiff == 2 || xdiff == -2) {                                           \
      bits castle_mask = X_ONLY(sqto) > 4 ? (mto >> 1) ^ (mto << 1)            \
                                          : (mto << 1) ^ (mto >> 2);           \
      side_sets[!!SIDEOF(X)] ^= castle_mask;                                   \
      piece_sets[rook] ^= castle_mask;                                         \
                                                                               \
      int cfrom = Y_ONLY(sqfrom), cto;                                         \
      if (xdiff > 0)                                                           \
        cfrom += 7, cto = cfrom - 2;                                           \
      else                                                                     \
        cto = cfrom + 3;                                                       \
      std::swap(pieces[cfrom], pieces[cto]);                                   \
      move_piece_hash(cfrom, cto, rook | SIDEOF(X));                           \
    }                                                                          \
  }

static bits zobrist_piece_table[64][16], zobrist_pawn_leap_table[64],
    zobrist_flag_table[8];

#include <random>

board::board() {
  move_stack.reserve(128);

  {
    static bool zobrist_initialized{};
    static spinlock zobrist_lock{};

    std::lock_guard<spinlock> guard(zobrist_lock);

    if (!zobrist_initialized) {
      std::mt19937_64 e(339532);

      for (int i = 0; i < 64; i++)
        for (int j = 0; j < 16; j++)
          zobrist_piece_table[i][j] = e();

      for (int i = 1; i < 64; i++)
        zobrist_pawn_leap_table[i] = e();

      for (int i = 0; i < 8; i++)
        zobrist_flag_table[i] = e();

      zobrist_initialized = true;
    }
  }
}

FORCEINLINE void board::move_piece_hash(int sqfrom, int sqto, uint8_t piece) {
  hash ^= zobrist_piece_table[sqfrom][piece];
  hash ^= zobrist_piece_table[sqto][piece];
}

FORCEINLINE void board::toggle_piece_hash(int sq, uint8_t piece) {
  hash ^= zobrist_piece_table[sq][piece];
}

FORCEINLINE void board::toggle_piece_hash(int sq, uint8_t piece1,
                                          uint8_t piece2) {
  hash ^= zobrist_piece_table[sq][piece1] ^ zobrist_piece_table[sq][piece2];
}

FORCEINLINE void board::flip_castling_rights(int index) {
  castling_rights_lost[index] ^= 1;
  hash ^= zobrist_flag_table[index];
}

FORCEINLINE void board::change_castling_rights(move &m, int index, bool lost) {
  bool old = castling_rights_lost[index];
  castling_rights_lost[index] = lost;
  m |= move(old ^ lost) << (CASTLING_BASE_BIT + index);

  if (old != lost)
    hash ^= zobrist_flag_table[index];
}

FORCEINLINE void board::flip_side() {
  side_to_move ^= 1;
  hash ^= zobrist_flag_table[zobrist_stm_flag];
}

static const char rook_pos_to_castling_rights_index_table[64] = {
    0,  -1, -1, -1, -1, -1, -1, 1,  -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 2,  -1, -1, -1, -1, -1, -1, 3};

FORCEINLINE bool board::is_rook_eligible_for_castling(int sq) {
  const char entry = rook_pos_to_castling_rights_index_table[sq];

  return entry >= 0 && !castling_rights_lost[(unsigned)entry];
}

void board::make_move(move m, bool no_checks) {
  if (!m) {
    move_stack.push_back(0);
    flip_side();
    return;
  }

  uint8_t sqfrom = MOVE_FROM(m), sqto = MOVE_TO(m);

  uint8_t from = pieces[sqfrom], to = pieces[sqto];
  bits mfrom = bits(1) << sqfrom, mto = bits(1) << sqto;
  int side_factor = side_to_move * 2 - 1;

  CASTLE(from)

  // Remove captured piece
  uint8_t captured = to;
  uint8_t sqcaptured = sqto;

  // Adjust potential right to en passant
  uint8_t pawn_leap = 0;

  if (PTYPE(from) == pawn && Y_COORD(sqfrom) - Y_COORD(sqto) == side_factor * 2)
    m |= SET_PAWN_LEAP(pawn_leap = sqto);

  move lm = move_stack.size() ? move_stack.back() : 0;

  int prev_leap = !CAPTURE(lm) ? PAWN_LEAP(lm) : 0;
  hash ^=
      zobrist_pawn_leap_table[pawn_leap] ^ zobrist_pawn_leap_table[prev_leap];

  // This is an en passant capture
  if (!captured && PTYPE(from) == pawn && X_ONLY(sqfrom) != X_ONLY(sqto))
    captured = pieces[sqcaptured = sqto + 8 * side_factor];

  int side = !!SIDEOF(from);

  if (captured) {
    bits mcaptured = bits(1) << sqcaptured;
    int cap_side = !!SIDEOF(captured);

    piece_sets[PTYPE(captured)] &= ~mcaptured;
    side_sets[cap_side] &= ~mcaptured;
    pieces[sqcaptured] = 0;
    toggle_piece_hash(sqcaptured, S_AND_T(captured));

    m |= SET_CAPTURE_SQUARE(sqcaptured);
    m |= SET_CAPTURE(captured);

    // If captured piece was a rook that hadn't moved,
    // revoke castling rights for its king on that side
    if (PTYPE(captured) == rook && is_rook_eligible_for_castling(sqcaptured))
      change_castling_rights(m, cap_side * 2 + !!X_ONLY(sqcaptured), true);
  }

  // Move the piece

  pieces[sqto] = pieces[sqfrom];
  pieces[sqfrom] = 0;
  move_piece_hash(sqfrom, sqto, S_AND_T(pieces[sqto]));

  // Adjust castling rights

  if (PTYPE(from) == king) {
    change_castling_rights(m, side * 2 + queenside, true);
    change_castling_rights(m, side * 2 + kingside, true);
  } else if (PTYPE(from) == rook && is_rook_eligible_for_castling(sqfrom))
    change_castling_rights(m, side * 2 + (X_ONLY(sqfrom) >= KING_FILE), true);

  piece_sets[PTYPE(from)] ^= mfrom;
  side_sets[!!SIDEOF(from)] ^= mfrom ^ mto;
  uint8_t p = PROMOTION(m);
  if (p) {
    toggle_piece_hash(sqto, S_AND_T(from), p | SIDEOF(from));
    pieces[sqto] ^= PTYPE(pieces[sqto]) ^ p;
  } else
    p = PTYPE(from);

  piece_sets[p] |= mto;

  move_stack.push_back(m);
  flip_side();
}

void board::unmake_move(bool no_checks) {
  move m = move_stack.back();
  move_stack.pop_back();
  flip_side();

  if (!m)
    return;

  uint8_t sqfrom = MOVE_FROM(m), sqto = MOVE_TO(m);
  uint8_t from = pieces[sqfrom], to = pieces[sqto];
  bits mfrom = bits(1) << sqfrom, mto = bits(1) << sqto;

  // Undo castling
  CASTLE(to)

  uint8_t pawn_leap = !CAPTURE(m) ? PAWN_LEAP(m) : 0;

  // Adjust potential right to en passant
  move lm = move_stack.size() ? move_stack.back() : 0;

  int prev_leap = !CAPTURE(lm) ? PAWN_LEAP(lm) : 0;
  hash ^=
      zobrist_pawn_leap_table[pawn_leap] ^ zobrist_pawn_leap_table[prev_leap];

  int side = !!SIDEOF(to);

  // Move piece back to its original square
  // and put an empty square in its place for now
  move_piece_hash(sqfrom, sqto, S_AND_T(to));
  pieces[sqfrom] = pieces[sqto];
  pieces[sqto] = 0;

  piece_sets[PTYPE(to)] &= ~mto;
  side_sets[!!SIDEOF(to)] ^= mfrom ^ mto;

  // Maybe restore castling rights
  if (m >> CASTLING_BASE_BIT) {
    for (int i = 0; i < 4; i++)
      if (m & (move(1) << (CASTLING_BASE_BIT + i)))
        flip_castling_rights(i);
  }

  // Maybe undo promotion
  uint8_t p = PROMOTION(m);
  if (p) {
    toggle_piece_hash(sqfrom, S_AND_T(to), (p = pawn) | SIDEOF(to));
    pieces[sqfrom] ^= PTYPE(pieces[sqfrom]) ^ p;
  } else
    p = PTYPE(to);

  piece_sets[p] |= mfrom;

  // Restore captured piece
  if (auto cap_piece = CAPTURE(m)) {
    auto cap_position = CAPTURE_SQUARE(m);

    bits capmask = bits(1) << cap_position;

    pieces[cap_position] = cap_piece;
    toggle_piece_hash(cap_position, S_AND_T(cap_piece));
    side_sets[!!SIDEOF(cap_piece)] |= capmask;

    piece_sets[PTYPE(cap_piece)] |= capmask;
  }
}

std::string piece_to_char(piece_state piece) {
  static char chars[] = {'.', 'p', 'k', 'n', 'b', 'r', 'q'};
  char c = chars[PTYPE(piece)];

  if (SIDEOF(piece))
    c = toupper(c);

  return ansi_colored(std::string(1, c), SIDEOF(piece) ? 92 : piece ? 32 : 39);
}

void board::create_piece(int sq, piece_state piece) {
  pieces[sq] = piece;
  side_sets[!!SIDEOF(piece)] |= bits(1) << sq;
  piece_sets[PTYPE(piece)] |= bits(1) << sq;
  toggle_piece_hash(sq, S_AND_T(piece));
}

void board::create_piece_pair(int sq, piece_state piece_type, bool v, bool h) {
  create_piece(sq, piece_type | black);
  if (v)
    create_piece(sq ^ 0b111000, piece_type | white);

  if (h) {
    create_piece(sq ^ 0b000111, piece_type | black);
    if (v)
      create_piece(sq ^ 0b111111, piece_type | white);
  }
}

void board::starting_position() {
  import_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");

  //  import_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq
  //  -"); import_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -");
  // import_fen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0
  // 1"); import_fen(
  //     "r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1");
  //  import_fen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8");
  //  import_fen("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1
  //  w
  //  - - 0 10");
  // import_fen("1r6/4r1pk/5q1p/p1pp1p1Q/P3p3/1PPbP1PP/R2N1P2/2R4K w - - 1 30");
}

#include "seq_movegen.hh"
#include <iostream>

#define DEBUG_CONSISTENCY 1

#include <cstdlib>
#include <malloc.h>
#include <omp.h>

board::perft_state board::perft_backend(int depth, bool divide,
                                        std::ostream &out) {
  if (depth == 1)
    return perft_internal(1);

  perft_state result;

  std::vector<board> boards;

  size_t len = 0;
  move moves_arr[128], legal_moves_arr[128];
  move *moves = moves_arr;
  move *legal_moves = legal_moves_arr;

  bulk_move_generator mg(*this);
  move *end = mg.gen_all(moves);

  while (moves < end) {
    boards.push_back(*this);
    boards.back().make_move(*moves);
    if (!boards.back().in_check(boards.back().side_to_move ^ 1)) {
      *legal_moves++ = *moves;
      len++;
    } else
      boards.pop_back();
    moves++;
  }

  std::vector<board::perft_state> results(len);

  #pragma omp parallel for
  for (size_t i = 0; i < len; i++)
    results[i] = boards[i].perft_internal(depth - 1);

  for (size_t i = 0; i < results.size(); i++) {
    if (divide) {
      out << format(
          "({}; {}) -> ({}; {}): {} nodes, {} captures, {} checks, "
          "{} castles, {} promotions\n",
          X_COORD(MOVE_FROM(legal_moves_arr[i])),
          Y_COORD(MOVE_FROM(legal_moves_arr[i])),
          X_COORD(MOVE_TO(legal_moves_arr[i])),
          Y_COORD(MOVE_TO(legal_moves_arr[i])),

          results[i].nodes, results[i].captures, results[i].checks,
          results[i].castles, results[i].promotions);
    }

    result += results[i];
  }

  return result;
}

#include "eval.hh"

board::perft_state board::perft_internal(int depth) {
  perft_state result;

  move moves_arr[128];
  move *moves = moves_arr;

  bulk_move_generator mg(*this);
  move *end = mg.gen_all(moves);

  while (moves < end) {
#if DEBUG_CONSISTENCY
    bits old_hash = hash;
    board old_board = *this;

    if (!bulk_move_generator::is_move_pseudolegal(old_board, *moves)) {
      logger::err << format("Move not pseudolegal: {}!\n",
    move_to_algebraic(*moves));

      logger::err << "State before move: \n";
      old_board.print_state(logger::err);
    }

#endif
    make_move(*moves);
#if DEBUG_CONSISTENCY
    board board_after_move = *this;
#endif

    if (!in_check(side_to_move ^ 1)) {
      if (depth == 1)
        result += perft_state{
            .nodes = 1,
            .checks = in_check(side_to_move),
            .captures = !!CAPTURE(move_stack.back()),
            .castles =
                PTYPE(pieces[MOVE_TO(*moves)]) == king &&
                (X_ONLY(MOVE_FROM(*moves)) - X_ONLY(MOVE_TO(*moves)) == +2 ||
                 X_ONLY(MOVE_FROM(*moves)) - X_ONLY(MOVE_TO(*moves)) == -2),
            .promotions = !!PROMOTION(*moves),
            .eval = 0
        };
      else
        result += perft_internal(depth - 1);
    }

    unmake_move();
    moves++;

#if DEBUG_CONSISTENCY
    if (side_sets[0] != old_board.side_sets[0] ||
        side_sets[1] != old_board.side_sets[1] ||
        piece_sets[pawn] != old_board.piece_sets[pawn] ||
        piece_sets[knight] != old_board.piece_sets[knight] ||
        piece_sets[bishop] != old_board.piece_sets[bishop] ||
        piece_sets[rook] != old_board.piece_sets[rook] ||
        piece_sets[queen] != old_board.piece_sets[queen] ||
        piece_sets[king] != old_board.piece_sets[king] || hash != old_hash) {
      logger::err << "State mismatch!\n";
      logger::err << "State before move: \n";
      old_board.print_state(logger::err);
      logger::err << "State after move: \n";
      board_after_move.print_state(logger::err);
      logger::err << "State after unmaking move: \n";
      print_state(logger::err);
    }
#endif
  }

  return result;
}

#include <sstream>
#include <unordered_map>

bool board::import_fen(const std::string &fen) {
  std::stringstream ss(fen);
  return import_fen(ss);
}

bool board::import_fen(std::istream &stream) {
  *this = board();

  const static std::unordered_map<char, piece_state> piece_table = {
      {'r', rook | black},   {'R', rook | white},   {'q', queen | black},
      {'Q', queen | white},  {'p', pawn | black},   {'P', pawn | white},
      {'k', king | black},   {'K', king | white},   {'b', bishop | black},
      {'B', bishop | white}, {'n', knight | black}, {'N', knight | white},
  };

  stream >> std::noskipws;

  for (int y = 0; y < 8; y++) {
    int x = 0;

    while (x < 8) {
      char c;
      stream >> c;

      if (std::isdigit(c))
        x += c - '0';
      else {
        auto f = piece_table.find(c);
        if (f != piece_table.end())
          create_piece(x++ | y * 8, f->second);
        else
          return false;
      }
    }

    if (y < 7) {
      char slash;
      stream >> slash;
    }
  }

  stream >> std::ws;

  char stm;
  stream >> stm;

  if (stm == 'w') {
    side_to_move = 0;
    flip_side();
  }

  stream >> std::ws;

  for (int right = 0; right < 4; right++)
    flip_castling_rights(right);

  char c;

  for (bool stop = false; !stop;) {
    stream >> c;

    switch (c) {
    case 'k':
      flip_castling_rights(0 * 2 + 1);
      break;
    case 'K':
      flip_castling_rights(1 * 2 + 1);
      break;
    case 'q':
      flip_castling_rights(0 * 2 + 0);
      break;
    case 'Q':
      flip_castling_rights(1 * 2 + 0);
      break;
    default:
      stream.unget();
      stop = true;
      break;
    }
  }

  stream >> std::ws;

  stream >> c;

  if (c != '-') {
    stream.unget();

    int pawn_leap = parse_algebraic_from_stream(stream);

    if (pawn_leap != -1) {
      move_stack.push_back(pawn_leap);
      hash ^= zobrist_pawn_leap_table[pawn_leap];
    } else
      return false;
  }

  int moves1, moves2;

  stream >> moves1 >> moves2;

  return true;
}

#include <chrono>

void board::perft(int depth, bool divide, std::ostream &out) {
  auto start = std::chrono::high_resolution_clock::now();

  perft_state result = perft_backend(depth, divide, out);

  auto end = std::chrono::high_resolution_clock::now();

  auto diff = end - start;
  auto diff_ms = std::chrono::duration<double, std::milli>(diff).count();

  out << format("Took {} ms ({} Mn/s). "
                     "{} nodes, {} captures, {} checks, "
                     "{} castles, {} promotions, {} eval\n",
                     diff_ms, result.nodes / diff_ms / 1.e3, result.nodes,
                     result.captures, result.checks, result.castles,
                     result.promotions, result.eval);
}

#include "eval.hh"
#include "search.hh"

void board::debug_console(std::istream &in, std::ostream &out) {
  for (;;) {
    out << "> ";
    std::string cmd;
    in >> cmd;

    if (cmd == "p" || cmd == "print") {
      print_state(out);
    } else if (cmd == "m" || cmd == "make") {
      uint32_t x1, y1, x2, y2;
      while (!(in >> x1 >> y1 >> x2 >> y2))
        ;

      move m{x1 + y1 * 8 + x2 * 64 + y2 * 512};
      make_move(m);
    } else if (cmd == "islegal") {
      move m = parse_move_from_stream(in);

      if (bulk_move_generator::is_move_pseudolegal(*this, m)) {
        make_move(m);
        if (in_check(side_to_move))
          out << "Pseudolegal but leaves king in check\n";
        else
          out << "Legal\n";
        unmake_move();
      } else
        out << "Move is not pseudolegal\n";
    } else if (cmd == "u" || cmd == "unmake")
      unmake_move();
    else if (cmd == "lm" || cmd == "list_moves") {
      move_generator gen(*this);
      gen.start();

      move m;
      while ((m = gen.next()))
        out << move_to_algebraic(m) << '\n';
    } else if (cmd == "perft" || cmd == "div" || cmd == "divide") {
      int depth;
      in >> depth;
      perft(depth, cmd != "perft", out);
    } else if (cmd == "eval")
      std::printf("%i\n", eval::myeval(*this));
    else if (cmd == "best") {
      int tl, depth;

      in >> tl;
      if (in.peek() == '\n')
        depth = 100;
      else
        in >> depth;

      search::reset_nps();

      execution_limiter el;

      std::thread limit([&el, &tl]() mutable {
        std::this_thread::sleep_for(std::chrono::milliseconds(tl));
        el.stop = true;
      });

      move best;

      search::go(*this, &best, depth, 16, el, logger::null, out);

      out << format("best = {}\n", move_to_algebraic(best));
      out << format("nodes per second: {}\n", search::nps());
      out << format("average branching factor: {}\n",
                         search::avg_branching_factor());

      limit.detach();
    }
  }
}

void board::print_state(std::ostream &out) const {
  out << "Hash: " << ansi_colored(format("{:#x}\n", hash), 93);
  out << format(
      "Castling rights lost: black [Q={} K={}]; white [Q={} K={}]\n",
      castling_rights_lost[0], castling_rights_lost[1], castling_rights_lost[2],
      castling_rights_lost[3]);
  out << format("{} to move\n", side_to_move ? "White" : "Black");
  out << "Pieces:\n";

  for (int sq = 0; sq < 64; sq++) {
    out << piece_to_char(pieces[sq]);

    if (sq % 8 == 7)
      out << '\n';
  }

  out << "Bitboards:\n";

  out << format("{:^9}", "black");
  out << format("{:^9}", "white");
  out << format("{:^9}", "pawns");
  out << format("{:^9}", "kings");
  out << format("{:^9}", "knights");
  out << format("{:^9}", "bishops");
  out << format("{:^9}", "rooks");
  out << format("{:^9}\n", "queens");

  for (int y = 0; y < 8; y++) {
    auto print_bitboard = [y, &out, this](auto bb) {
      for (int x = 0; x < 8; x++) {
        bits mask = bits(1) << (x + y * 8);
        bool on = !!((bb)&mask);

        int color = !on ? 39 : (side_sets[1] & mask) ? 92 : 32;

        out << ansi_colored(std::string(1, on ? '1' : '.'), color);
      }

      out << ' ';
    };

    print_bitboard(side_sets[0]);
    print_bitboard(side_sets[1]);
    print_bitboard(piece_sets[pawn]);
    print_bitboard(piece_sets[king]);
    print_bitboard(piece_sets[knight]);
    print_bitboard(piece_sets[bishop]);
    print_bitboard(piece_sets[rook]);
    print_bitboard(piece_sets[queen]);

    out << '\n';
  }

  out.flush();
}