#include "transpose_benchmark.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>
#include <vector>

namespace {

const std::vector<std::pair<int, int>> kDefaultSizes = {
    {32, 32},     {512, 512},   {1024, 1024}, {2048, 2048}, {4096, 4096}, {8192, 8192},
    {1024, 4096}, {4096, 1024}, {2048, 8192}, {2080, 4128},
};

void usage(const char* prog) {
  std::fprintf(stderr,
               "Usage: %s [options]\n"
               "  --sizes LIST     comma-separated HxW sizes, e.g. 512x512,4096x1024\n"
               "                   (default: built-in representative set)\n"
               "  --square LIST    comma-separated square sizes, e.g. 512,1024,4096 (merged into sizes)\n"
               "  --kernels LIST   comma-separated kernel names (default: all, see --list-kernels)\n"
               "  --inputs N       random inputs per size (default 5)\n"
               "  --runs N         timed samples per (kernel,size,input) (default 100)\n"
               "  --warmup N       untimed warmup launches per (kernel,size,input) (default 10)\n"
               "  --seed S         RNG seed (default 42)\n"
               "  --out DIR        output directory (default results)\n"
               "  --list-kernels   print available kernel names and exit\n"
               "  --help           show this message\n",
               prog);
}

std::vector<std::string> split(const std::string& s, char sep) {
  std::vector<std::string> out;
  std::size_t start = 0;
  while (true) {
    const std::size_t pos = s.find(sep, start);
    if (pos == std::string::npos) {
      out.push_back(s.substr(start));
      break;
    }
    out.push_back(s.substr(start, pos - start));
    start = pos + 1;
  }
  return out;
}

bool parse_size(const std::string& tok, int& H, int& W) {
  std::size_t pos = tok.find('x');
  if (pos == std::string::npos) pos = tok.find('X');
  if (pos == std::string::npos) pos = tok.find('*');
  if (pos == std::string::npos) return false;
  H = std::atoi(tok.substr(0, pos).c_str());
  W = std::atoi(tok.substr(pos + 1).c_str());
  return H > 0 && W > 0;
}

}  // namespace

int main(int argc, char** argv) {
  BenchmarkConfig cfg;
  cfg.sizes = kDefaultSizes;
  bool have_sizes = false;

  for (int i = 1; i < argc; ++i) {
    const std::string arg = argv[i];
    auto next = [&](const char* name) -> const char* {
      if (i + 1 >= argc) {
        std::fprintf(stderr, "error: %s requires a value\n", name);
        std::exit(2);
      }
      return argv[++i];
    };
    if (arg == "--help" || arg == "-h") {
      usage(argv[0]);
      return 0;
    } else if (arg == "--list-kernels") {
      for (const auto& k : available_kernels()) std::printf("%s\n", k.c_str());
      return 0;
    } else if (arg == "--sizes") {
      cfg.sizes.clear();
      have_sizes = true;
      for (const auto& tok : split(next("--sizes"), ',')) {
        int H, W;
        if (!parse_size(tok, H, W)) {
          std::fprintf(stderr, "error: bad size '%s' (expected HxW)\n", tok.c_str());
          return 2;
        }
        cfg.sizes.push_back({H, W});
      }
    } else if (arg == "--square") {
      for (const auto& tok : split(next("--square"), ',')) {
        int H, W;
        if (!parse_size(tok + "x" + tok, H, W)) {
          std::fprintf(stderr, "error: bad square size '%s'\n", tok.c_str());
          return 2;
        }
        cfg.sizes.push_back({H, W});
      }
      have_sizes = true;
    } else if (arg == "--kernels") {
      cfg.kernels = split(next("--kernels"), ',');
    } else if (arg == "--inputs") {
      cfg.numInputs = std::atoi(next("--inputs"));
    } else if (arg == "--runs") {
      cfg.timedRuns = std::atoi(next("--runs"));
    } else if (arg == "--warmup") {
      cfg.warmupRuns = std::atoi(next("--warmup"));
    } else if (arg == "--seed") {
      cfg.seed = std::strtoull(next("--seed"), nullptr, 10);
    } else if (arg == "--out") {
      cfg.outDir = next("--out");
    } else {
      std::fprintf(stderr, "error: unknown option '%s'\n", arg.c_str());
      usage(argv[0]);
      return 2;
    }
  }
  (void)have_sizes;

  if (cfg.numInputs < 1 || cfg.timedRuns < 1 || cfg.warmupRuns < 0 || cfg.sizes.empty()) {
    std::fprintf(stderr, "error: --inputs >= 1, --runs >= 1, --warmup >= 0 required\n");
    return 2;
  }

  std::printf("config: %zu sizes, kernels=[", cfg.sizes.size());
  if (cfg.kernels.empty()) {
    std::printf("all");
  } else {
    for (std::size_t i = 0; i < cfg.kernels.size(); ++i)
      std::printf("%s%s", i ? "," : "", cfg.kernels[i].c_str());
  }
  std::printf("], inputs=%d, runs=%d, warmup=%d, seed=%llu, out=%s\n", cfg.numInputs,
              cfg.timedRuns, cfg.warmupRuns, cfg.seed, cfg.outDir.c_str());
  for (const auto& s : cfg.sizes) std::printf("  size %dx%d\n", s.first, s.second);

  return run_benchmark(cfg);
}
