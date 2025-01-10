#pragma once

#include <atomic>
#include <format>
#include <fstream>
#include <numeric>
#include <random>

typedef uint64_t bits;

#define POP_LSB(x) ((x) &= (x) - 1)
#define FORCEINLINE inline

FORCEINLINE unsigned long ctz64(uint64_t l) {
  unsigned long i = __builtin_ffsll(l) - 1;

  return i;
}

#define POPCNT64(x) (int)(std::popcount(x))

using std::format;

struct spinlock {
  std::atomic<bool> lock_ = {0};

  void lock() noexcept {
    for (;;) {
      if (!lock_.exchange(true, std::memory_order_acquire))
        return;
      while (lock_.load(std::memory_order_relaxed))
        ;
    }
  }

  bool try_lock() noexcept {
    return !lock_.load(std::memory_order_relaxed) &&
           !lock_.exchange(true, std::memory_order_acquire);
  }

  void unlock() noexcept { lock_.store(false, std::memory_order_release); }
};

constexpr inline std::string ansi_colored(const std::string &arr, int code) {
  return std::format("\033[{}m{}\033[m", code, arr);
}

uint32_t random_int(uint32_t min, uint32_t max);

// #define DEBUG_LOG std::printf
#define DEBUG_LOG

namespace logger {
extern std::ofstream err;

extern class null_stream : public std::ostream {
  class null_buffer : public std::streambuf {
  public:
    int overflow(int c) override { return c; }
  } buffer_;

public:
  null_stream() : std::ostream(&buffer_) {}
} null;
} // namespace logger