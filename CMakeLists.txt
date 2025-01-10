cmake_minimum_required(VERSION 3.10)
project(ChessServer LANGUAGES CXX)

set(CMAKE_BUILD_TYPE Release)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_FLAGS "-Wall -Wextra -fansi-escape-codes")
set(CMAKE_CXX_FLAGS_DEBUG "-g -fsanitize=undefined -fsanitize=address")
set(CMAKE_CXX_FLAGS_RELEASE "-g -O3 -flto")
find_package(OpenMP)
add_executable(ChessServer
  board.cc
  server.cc
  lut.cc
  seq_movegen.cc
  bulk_movegen.cc
  myeval.cc
  search.cc
  tt.cc
  pesto.cc
  uci.cc
  move_picker.cc
  logging.cc
  random.cc)
target_link_libraries(ChessServer PUBLIC OpenMP::OpenMP_CXX)