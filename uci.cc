#include "uci.hh"
#include "board.hh"
#include "search.hh"
#include "util.hh"
#include <format>
#include <fstream>
#include <iostream>
#include <sstream>
#include <thread>

void uci::run(std::istream &inp, std::ostream &outp) {
  board bd;

  static const char name[] = "ChessEngine";
  static const char author[] = "n";

  for (;;) {
    std::string scratch;
    std::getline(inp, scratch);
    std::stringstream ss(scratch);
    std::istream &inp = ss;

    std::string scmd;
    inp >> scmd;

    if (scmd == "uci") {
      outp << format("id name {}\n", name);
      outp << format("id author {}\n", author);
      outp << "uciok\n";
    } else if (scmd == "isready")
      outp << "readyok\n";
    else if (scmd == "position") {
      if (inp.peek() == EOF)
        continue;

      char c;
      inp >> c;
      inp.unget();

      if (c == 's') {
        std::string s;
        inp >> s;
        bd.starting_position();
      } else {
        inp >> std::noskipws;
        bd.import_fen(inp);
        inp >> std::skipws;
      }

      if (inp.eof())
        continue;

      inp >> c;
      inp.unget();

      if (c == 'm') {
        std::string moves;
        inp >> moves;

        while (!inp.eof()) {
          move m = parse_move_from_stream(inp);
          if (!m)
            break;
          bd.make_move(m);
        }
      }
    } else if (scmd == "go") {
      logger::err << "go acknowledged\n";
      logger::err.flush();
      execution_limiter el;

      int tl = 3000;
      move best;

      std::thread limit([&el, &tl]() mutable {
        std::this_thread::sleep_for(std::chrono::milliseconds(tl));
        el.stop = true;
      });

      search::go(bd, &best, 100, 16, el, outp, logger::err);

      logger::err << format("bestmove {}\n", move_to_algebraic(best));
      outp << format("bestmove {}\n", move_to_algebraic(best));
      logger::err.flush();

      limit.detach();
    } else if (scmd == "quit") {
      std::exit(0);
    }
  }
}