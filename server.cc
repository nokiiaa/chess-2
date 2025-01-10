#include <fstream>
#include <iostream>

#include "lut.hh"
#include "board.hh"
#include "tt.hh"
#include "uci.hh"

int main(const int argc, const char *argv[])
{
    if (argc > 1)
        logger::err = std::ofstream(argv[1]);
    else 
        logger::err = std::ofstream("chess_server_log.txt");

    ttable::init_global(26);
    lut::generate_data();

    uci::run(std::cin, std::cout);

    board b;
    b.starting_position();
    b.debug_console(std::cin, std::cout);

    return 0;
}
