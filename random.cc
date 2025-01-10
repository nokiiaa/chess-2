#include "util.hh"
#include <random>

uint32_t random_int(uint32_t min, uint32_t max) {
  static std::atomic<int> n_thread = 0;
  static thread_local std::mt19937 generator(n_thread++);

  return std::uniform_int_distribution<uint32_t>(min, max)(generator);
}
