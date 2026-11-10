// benchmark.cu — correctness + timed sampling driver (kernel-registry design).
//
// Pipeline, per the experimental protocol:
//   for each size (H x W):
//     for each random draw:
//       - generate a random input A
//       - run CPU reference + each registered GPU kernel, verify equality
//       - (untimed) warmup runs
//       - many timed runs (with optional L2 flush between samples) -> CSV
//
// To add a new kernel:
//   1. Write the __global__ function in kernels.cuh.
//   2. Add one entry to allKernels() below.
//   3. (Optional) Add its name to --impl to select it.
#include "common.h"
#include "kernels.cuh"

#include <algorithm>
#include <cerrno>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <fstream>
#include <functional>
#include <numeric>
#include <random>
#include <sstream>
#include <string>
#include <vector>

#include <sys/stat.h>
#include <sys/types.h>

#include <cublas_v2.h>
// cuBLAS handle (single device, created once in main()).
static cublasHandle_t gCublas = nullptr;

// ===========================================================================
// Kernel registry
// ===========================================================================
// Each entry describes one implementation to benchmark.
struct KernelSpec {
  const char* name;
  // The launch lambda encapsulates the <<<>>> syntax so the driver
  // stays kernel-agnostic.
  std::function<void(const float*, float*, int, int, cudaStream_t)> launch;
};

static std::vector<KernelSpec> allKernels() {
  return {
    // --- naive ---
    {"naiveQwen",
     [](const float* A, float* B, int H, int W, cudaStream_t s) {
       dim3 grid = gridFor(H, W, TILE_DIM);
       dim3 block(TILE_DIM, TILE_DIM);
       transposeNaiveQwen<<<grid, block, 0, s>>>(A, B, H, W);
     }},

    // --- tiled (bank-conflict-free) ---
    {"tiledQwen",
     [](const float* A, float* B, int H, int W, cudaStream_t s) {
       dim3 grid = gridFor(H, W, TILE_DIM);
       dim3 block(TILE_DIM, TILE_DIM);
       transposeNoBankConflictsQwen<<<grid, block, 0, s>>>(A, B, H, W);
     }},

    // --- tiled reference (bank-conflict-free) ---
    {"ref",
     [](const float* A, float* B, int H, int W, cudaStream_t s) {
       dim3 grid = gridFor(H, W, TILE_DIM);
       dim3 block(TILE_DIM, BLOCK_ROWS);
       transposeNoBankConflicts<<<grid, block, 0, s>>>(A, B, H, W);
     }},

    // --- OptiTrust code ---
    {"OptiTrust",
     [](const float* A, float* B, int H, int W, cudaStream_t s) {
       dim3 grid = (H / TILE_DIM) * (W / TILE_DIM);
       dim3 block(BLOCK_ROWS * TILE_DIM);
       transposeOptiTrust<<<grid, block, sizeof(float) * (32 * 32), s>>>(A, B, H, W);
     }},

    // --- OptiTrust code (simplified) ---
    {"OptiTrust-simpl",
     [](const float* A, float* B, int H, int W, cudaStream_t s) {
       dim3 grid = (H / TILE_DIM) * (W / TILE_DIM);
       dim3 block(BLOCK_ROWS * TILE_DIM);
       transposeOptiTrustSimplified<<<grid, block, sizeof(float) * (32 * 32), s>>>(A, B, H, W);
     }},

    {"OptiTrust-noconflicts",
     [](const float* A, float* B, int H, int W, cudaStream_t s) {
       dim3 grid = (H / TILE_DIM) * (W / TILE_DIM);
       dim3 block(BLOCK_ROWS * TILE_DIM);
       transposeOptiTrustSimplifiedNoConflicts<<<grid, block, sizeof(float) * (32 * 33), s>>>(A, B, H, W);
     }},

    {"cublas",
     [](const float* A, float* B, int H, int W, cudaStream_t s) {
      float alpha = 1.0f, beta = 0.0f;

      // A is stored W×H in column-major (it's H×W row-major).
      // We want op(A) = Aᵀ, which gives H×W = m×n.
      // B is not accessed (beta = 0); we point it at dB harmlessly.
      cublasSgeam(gCublas,
          alpha, A, /*lda=*/W, CUBLAS_OP_T,
          beta,  B, /*ldb=*/H, CUBLAS_OP_N,
          B,   /*ldc=*/H,
          /*m=*/H, /*n=*/W);
    }};
}

// ===========================================================================
// CPU reference & L2 flush (unchanged)
// ===========================================================================
static void transposeRef(const float* A, float* B, int H, int W) {
  for (int y = 0; y < H; ++y)
    for (int x = 0; x < W; ++x)
      B[x * H + y] = A[y * W + x];
}

static __global__ void l2Fill(float* p, size_t n) {
  size_t i = (size_t)blockIdx.x * blockDim.x + threadIdx.x;
  if (i < n) p[i] = 0.f;
}

static void flushL2(float* buf, size_t n_elems, cudaStream_t s) {
  size_t block = 256;
  size_t grid = (n_elems + block - 1) / block;
  l2Fill<<<(int)grid, (int)block, 0, s>>>(buf, n_elems);
}

// ===========================================================================
// Options & CLI
// ===========================================================================

struct Options {
  int  iters   = 30;
  int  draws   = 2;
  int  warmup  = 5;
  long baseSeed = 0;
  bool l2flush = true;
  double tol  = 1e-6;
  std::string outDir = "results";
  std::string impl = "all";  // "all" or comma-separated kernel names
  std::vector<Config> sizes;
  bool hasExplicitSizes = false;
};

static std::vector<Config> defaultSizes() {
  return {
    {512,512},{1024,1024},{2048,2048},{4096,4096},
    // {8192,8192},
    // {16384,16384},
    {256,2048},{2048,256},{1024,4096},{4096,1024},
    // {256,8192},{8192,256}
  };
}

static bool parseSize(const std::string& s, Config& out) {
  size_t p = s.find('x');
  if (p == std::string::npos) return false;
  try { out.H = std::stoi(s.substr(0, p)); out.W = std::stoi(s.substr(p + 1)); }
  catch (...) { return false; }
  return out.H > 0 && out.W > 0;
}

static void parseArgs(int argc, char** argv, Options& o) {
  auto arg = [&](int& i, const char* name) -> std::string {
    if (i + 1 < argc) return argv[++i];
    fprintf(stderr, "missing value for %s\n", name); exit(2);
  };
  for (int i = 1; i < argc; ++i) {
    std::string a = argv[i];
    if (a == "--iters")       o.iters   = std::stoi(arg(i, a.c_str()));
    else if (a == "--draws")  o.draws   = std::stoi(arg(i, a.c_str()));
    else if (a == "--warmup") o.warmup  = std::stoi(arg(i, a.c_str()));
    else if (a == "--seed")   o.baseSeed= std::stol(arg(i, a.c_str()));
    else if (a == "--tol")    o.tol     = std::stod(arg(i, a.c_str()));
    else if (a == "--out")    o.outDir  = arg(i, a.c_str());
    else if (a == "--impl")   o.impl    = arg(i, a.c_str());
    else if (a == "--no-l2flush") o.l2flush = false;
    else if (a == "--sizes") {
      o.hasExplicitSizes = true;
      std::string list = arg(i, a.c_str());
      size_t start = 0;
      while (start < list.size()) {
        size_t c = list.find(',', start);
        std::string tok = (c == std::string::npos)
                              ? list.substr(start)
                              : list.substr(start, c - start);
        Config cfg;
        if (!parseSize(tok, cfg)) {
          fprintf(stderr, "bad size token '%s'\n", tok.c_str()); exit(2);
        }
        o.sizes.push_back(cfg);
        if (c == std::string::npos) break;
        start = c + 1;
      }
    }
    else if (a == "-h" || a == "--help") {
      fprintf(stderr,
        "options: --iters N --draws N --warmup N --seed S --tol F "
        "--out DIR --impl NAMES --no-l2flush --sizes 'HxW,...'\n"
        "  --impl: 'all' (default) or comma-separated kernel names "
        "(e.g. 'naive,tiled,vectorized')\n");
      exit(0);
    }
  }
  if (!o.hasExplicitSizes) o.sizes = defaultSizes();
}

// ===========================================================================
// Main
// ===========================================================================
int main(int argc, char** argv) {
  Options o;
  parseArgs(argc, argv, o);

  // Build the full kernel list, then filter by --impl.
  auto all = allKernels();
  std::vector<const KernelSpec*> active;

  if (o.impl == "all") {
    for (auto& k : all) active.push_back(&k);
  } else {
    // Parse comma-separated names.
    size_t start = 0;
    while (start <= o.impl.size()) {
      size_t c = o.impl.find(',', start);
      std::string name = (c == std::string::npos)
                             ? o.impl.substr(start)
                             : o.impl.substr(start, c - start);
      bool found = false;
      for (auto& k : all) {
        if (k.name == name) { active.push_back(&k); found = true; }
      }
      if (!found) {
        fprintf(stderr, "unknown kernel '%s'. Available: ", name.c_str());
        for (auto& k : all) fprintf(stderr, "%s ", k.name);
        fprintf(stderr, "\n");
        return 2;
      }
      if (c == std::string::npos) break;
      start = c + 1;
    }
  }

  if (active.empty()) {
    fprintf(stderr, "no kernels selected\n");
    return 2;
  }

  // Device info.
  cudaDeviceProp prop;
  CUDA_CHECK(cudaGetDeviceProperties(&prop, 0));
  std::string ccStr = "sm_" + std::to_string(prop.major) + std::to_string(prop.minor);
  const int cudart = CUDART_VERSION;
  std::string cudartStr = std::to_string(cudart / 1000) + "." +
                          std::to_string(cudart / 10 % 100);

  // L2 flush buffer.
  size_t l2Bytes = std::max<size_t>(prop.l2CacheSize, 256u * 1024u * 1024u);
  float* flushBuf = nullptr;
  size_t flushN = 0;
  if (o.l2flush) {
    flushN = l2Bytes / sizeof(float);
    CUDA_CHECK(cudaMalloc(&flushBuf, flushN * sizeof(float)));
  }

  GpuTimer timer;
  cudaStream_t stream = 0;

  // cuBLAS handle.
  {
    cublasStatus_t st = cublasCreate(&gCublas);
    if (st != CUBLAS_STATUS_SUCCESS) {
      fprintf(stderr, "cublasCreate failed: %d\n", (int)st);
      return 1;
    }
    st = cublasSetStream(gCublas, stream);
    if (st != CUBLAS_STATUS_SUCCESS) {
      fprintf(stderr, "cublasSetStream failed: %d\n", (int)st);
      return 1;
    }
  }


  if (::mkdir(o.outDir.c_str(), 0755) != 0 && errno != EEXIST) {
    fprintf(stderr, "cannot create output dir '%s': %s\n",
            o.outDir.c_str(), strerror(errno));
    return 2;
  }
  std::ofstream samples(o.outDir + "/samples.csv", std::ios::trunc);
  std::ofstream corr(o.outDir + "/correctness.csv", std::ios::trunc);
  std::ofstream meta(o.outDir + "/meta.csv", std::ios::trunc);
  samples << "size_id,impl,H,W,draw,sample,time_us,gbps\n";
  corr    << "size_id,impl,H,W,draw,n,max_abs_diff,ok\n";

  meta << "key,value\n";
  {
    std::string impls;
    for (size_t i = 0; i < active.size(); ++i) {
      if (i) impls += ";";   // semicolons: keeps the CSV strictly 2-column
      impls += active[i]->name;
    }
    meta << "impls," << impls << "\n";
  }
  meta << "device," << prop.name << "\n"
       << "compute_capability," << ccStr << "\n"
       << "l2_cache_bytes," << prop.l2CacheSize << "\n"
       << "sm_count," << prop.multiProcessorCount << "\n"
       << "cuda_runtime," << cudartStr << "\n"
       << "iters," << o.iters << "\n"
       << "draws," << o.draws << "\n"
       << "warmup," << o.warmup << "\n"
       << "l2_flush," << (o.l2flush ? 1 : 0) << "\n"
       << "tol," << o.tol << "\n";

  int size_id = 0;
  long total_samples = 0;
  bool any_fail = false;

  for (const Config& cfg : o.sizes) {
    const int H = cfg.H, W = cfg.W;
    const long n = (long)H * W;
    printf("[size %02d] %dx%d  (%.1f MB/array)\n", size_id, H, W,
           n * sizeof(float) / (1024.0 * 1024.0));

    for (int draw = 0; draw < o.draws; ++draw) {
      std::mt19937 rng((unsigned)(o.baseSeed + draw));
      std::uniform_real_distribution<float> dist(0.0f, 1.0f);

      std::vector<float> hA(n), hCpuB(n), hGpuB(n);
      std::generate(hA.begin(), hA.end(), [&] { return dist(rng); });

      float *dA = nullptr, *dB = nullptr;
      CUDA_CHECK(cudaMalloc(&dA, n * sizeof(float)));
      CUDA_CHECK(cudaMalloc(&dB, n * sizeof(float)));
      CUDA_CHECK(cudaMemcpy(dA, hA.data(), n * sizeof(float),
                            cudaMemcpyHostToDevice));

      transposeRef(hA.data(), hCpuB.data(), H, W);
      const double bytes = 2.0 * n * sizeof(float);

      // ---- iterate over all active kernels ----
      for (const KernelSpec* ks : active) {
        auto launch = ks->launch;

        // Correctness: one run, compare to CPU reference.
        launch(dA, dB, H, W, stream);
        CUDA_CHECK(cudaGetLastError());
        CUDA_CHECK(cudaStreamSynchronize(stream));
        CUDA_CHECK(cudaMemcpy(hGpuB.data(), dB, n * sizeof(float),
                              cudaMemcpyDeviceToHost));
        double maxDiff = 0.0;
        for (long i = 0; i < n; ++i)
          maxDiff = std::max(maxDiff, (double)std::fabs(hGpuB[i] - hCpuB[i]));
        bool ok = maxDiff <= o.tol;
        if (!ok) any_fail = true;
        corr << size_id << "," << ks->name << "," << H << "," << W << ","
             << draw << "," << n << "," << maxDiff << "," << (ok ? 1 : 0) << "\n";
        if (!ok)
          fprintf(stderr, "  [%s] FAIL size %02d draw %d: max_diff=%g\n",
                  ks->name, size_id, draw, maxDiff);

        // Warmup (untimed).
        for (int w = 0; w < o.warmup; ++w) {
          if (o.l2flush) flushL2(flushBuf, flushN, stream);
          launch(dA, dB, H, W, stream);
        }
        CUDA_CHECK(cudaStreamSynchronize(stream));

        // Timed samples.
        for (int s = 0; s < o.iters; ++s) {
          if (o.l2flush) flushL2(flushBuf, flushN, stream);
          timer.begin();
          launch(dA, dB, H, W, stream);
          float ms = timer.end();
          double us = ms * 1000.0;
          double gbps = bytes / (us * 1e-6) / 1e9;
          samples << size_id << "," << ks->name << "," << H << "," << W << ","
                  << draw << "," << s << "," << us << "," << gbps << "\n";
          total_samples++;
        }
      }

      CUDA_CHECK(cudaFree(dA));
      CUDA_CHECK(cudaFree(dB));
    }
    size_id++;
  }

  meta << "total_samples," << total_samples << "\n";
  meta << "timestamp,"
       << std::chrono::duration_cast<std::chrono::seconds>(
              std::chrono::system_clock::now().time_since_epoch())
              .count() << "\n";
  meta << "all_correct," << (any_fail ? 0 : 1) << "\n";

  samples.close(); corr.close(); meta.close();
  if (flushBuf) CUDA_CHECK(cudaFree(flushBuf));
  if (gCublas) cublasDestroy(gCublas);

  printf("done: %ld samples written to %s/{samples,correctness,meta}.csv\n",
         total_samples, o.outDir.c_str());
  return any_fail ? 1 : 0;
}
