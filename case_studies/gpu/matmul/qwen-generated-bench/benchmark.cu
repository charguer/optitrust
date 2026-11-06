// benchmark.cu — GEMM: correctness + timed sampling driver (kernel-registry design).
//
// Pipeline, per the experimental protocol:
//   for each size (m × n × p):
//     for each random draw:
//       - generate random inputs A(m×p) and B(p×n)
//       - compute CPU reference C = A·B (double-precision, correctness only)
//       - for each registered GPU kernel:
//           run once, verify C against CPU reference (relative tolerance)
//           (untimed) warmup runs
//           many timed runs (with optional L2 flush between samples) -> CSV
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

// ===========================================================================
// Kernel registry
// ===========================================================================
// All kernels share the signature:
//   __global__ void kernel(float* C, const float* A, const float* B, int m, int n, int p)
struct KernelSpec {
  const char* name;
  std::function<void(float*, const float*, const float*, int, int, int, cudaStream_t)> launch;
};

static std::vector<KernelSpec> allKernels() {
  return {
    // --- OptiTrust (simplified): 32×32 tile, 32 threads, 1D grid ---
    {"OptiGPU",
     [](float* c, const float* a, const float* b, int m, int n, int p, cudaStream_t s) {
       int grid = (m / 32) * (n / 32);
       mmOptiTrust<<<grid, 32, sizeof(float)*(4*4*8 + 8*4*4), s>>>(c, a, b, m, n, p);
     }},

    // --- OptiTrust (simplified): 32×32 tile, 32 threads, 1D grid ---
    {"OptiGPU-simpl",
     [](float* c, const float* a, const float* b, int m, int n, int p, cudaStream_t s) {
       int grid = (m / 32) * (n / 32);
       mmOptiTrustSimplified<<<grid, 32, 0, s>>>(c, a, b, m, n, p);
     }},

    {"Siboehm6",
     [](float* c, const float* a, const float* b, int m, int n, int p, cudaStream_t s) {
      mmSiboehm6<<<SIBOEHM6_GRID_DIM(m, n), SIBOEHM6_BLOCK_DIM, (BM*BK + BK*BN) * sizeof(float_t)>>>(c, a, b, m, n, p);
    }},

    // --- Direct (naive): no shared memory, 1 output per thread ---
    {"direct",
     [](float* c, const float* a, const float* b, int m, int n, int p, cudaStream_t s) {
       int total = m * n;
       int block = 256;
       mmDirect<<<(total + block - 1) / block, block, 0, s>>>(c, a, b, m, n, p);
     }},

    // --- Tiled 16×16: classic CUDA sample ---
    {"tiled16",
     [](float* c, const float* a, const float* b, int m, int n, int p, cudaStream_t s) {
       dim3 grid(n / TILE16, m / TILE16);
       dim3 block(TILE16, TILE16);
       mmTiled16<<<grid, block, 0, s>>>(c, a, b, m, n, p);
     }},

    // --- Tiled 32×32 padded: bank-conflict-free ---
    {"tiled32",
     [](float* c, const float* a, const float* b, int m, int n, int p, cudaStream_t s) {
       dim3 grid(n / TILE32, m / TILE32);
       dim3 block(TILE32, TILE32);
       mmTiled32<<<grid, block, 0, s>>>(c, a, b, m, n, p);
     }},

    // --- Tiled 32×32 register-blocked (2×2 per thread) ---
    {"tiled32reg",
     [](float* c, const float* a, const float* b, int m, int n, int p, cudaStream_t s) {
       dim3 grid(n / TILE32b, m / TILE32b);
       dim3 block(16, 16);
       mmTiled32Reg<<<grid, block, 0, s>>>(c, a, b, m, n, p);
     }},

    // =========================================================================
    // ADD MORE KERNELS HERE.
    // =========================================================================
  };
}

// ===========================================================================
// CPU reference (double-precision, correctness only — not timed)
// ===========================================================================
static void gemmRef(const float* A, const float* B, double* C, int m, int n, int p) {
  for (int i = 0; i < m; ++i)
    for (int j = 0; j < n; ++j) {
      double s = 0.0;
      for (int k = 0; k < p; ++k)
        s += (double)A[i * p + k] * (double)B[k * n + j];
      C[i * n + j] = s;
    }
}

// ===========================================================================
// L2 flush
// ===========================================================================
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
  double tol = 1e-2;  // 1% — standard for float32 GEMM (p up to ~1024)
  std::string outDir = "results";
  std::string impl = "all";
  std::vector<Config> sizes;
  bool hasExplicitSizes = false;
};

// Default sizes: m×n×p, all multiples of 32 (required by all kernels).
// Square sweep + rectangular + tall/skinny.
static std::vector<Config> defaultSizes() {
  return {
    {128,128,128},
    {256,256,256},
    {512,512,512},
    {1024,1024,1024},
    {2048,2048,2048},
    // SLOWER TO BENCHMARK, dont do 50 iters etc?: {4096,4096,4096},
    // SLOWER TO BENCHMARK: {8192,8192,8192},
    // Rectangular
    {1024,2048,512},
    {512,2048,1024},
    {2048,1024,512},
    // Tall / skinny (stress occupancy)
    {4096,512,4096},
    {512,4096,4096},
  };
}

static bool parseSize(const std::string& s, Config& out) {
  // Format: "mxnxp"
  size_t p1 = s.find('x');
  size_t p2 = (p1 == std::string::npos) ? std::string::npos : s.find('x', p1 + 1);
  if (p1 == std::string::npos || p2 == std::string::npos) return false;
  try {
    out.m = std::stoi(s.substr(0, p1));
    out.n = std::stoi(s.substr(p1 + 1, p2 - p1 - 1));
    out.p = std::stoi(s.substr(p2 + 1));
  } catch (...) { return false; }
  return out.m > 0 && out.n > 0 && out.p > 0;
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
          fprintf(stderr, "bad size token '%s' (expected 'mxnxp')\n", tok.c_str());
          exit(2);
        }
        o.sizes.push_back(cfg);
        if (c == std::string::npos) break;
        start = c + 1;
      }
    }
    else if (a == "-h" || a == "--help") {
      fprintf(stderr,
        "options: --iters N --draws N --warmup N --seed S --tol F "
        "--out DIR --impl NAMES --no-l2flush --sizes 'mxnxp,mxnxp,...'\n"
        "  --impl: 'all' or comma-separated kernel names\n"
        "  m, n, p must be multiples of 32.\n");
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

  auto all = allKernels();
  std::vector<const KernelSpec*> active;
  if (o.impl == "all") {
    for (auto& k : all) active.push_back(&k);
  } else {
    size_t start = 0;
    while (start <= o.impl.size()) {
      size_t c = o.impl.find(',', start);
      std::string name = (c == std::string::npos)
                             ? o.impl.substr(start)
                             : o.impl.substr(start, c - start);
      bool found = false;
      for (auto& k : all)
        if (k.name == name) { active.push_back(&k); found = true; }
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
  if (active.empty()) { fprintf(stderr, "no kernels selected\n"); return 2; }

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

  if (::mkdir(o.outDir.c_str(), 0755) != 0 && errno != EEXIST) {
    fprintf(stderr, "cannot create output dir '%s': %s\n",
            o.outDir.c_str(), strerror(errno));
    return 2;
  }
  std::ofstream samples(o.outDir + "/samples.csv", std::ios::trunc);
  std::ofstream corr(o.outDir + "/correctness.csv", std::ios::trunc);
  std::ofstream meta(o.outDir + "/meta.csv", std::ios::trunc);
  samples << "size_id,impl,m,n,p,draw,sample,time_us,tflops\n";
  corr    << "size_id,impl,m,n,p,draw,max_rel_err,ok\n";

  meta << "key,value\n";
  {
    std::string impls;
    for (size_t i = 0; i < active.size(); ++i) {
      if (i) impls += ";";
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
    const int m = cfg.m, n = cfg.n, p = cfg.p;
    printf("[size %02d] %dx%dx%d  (%.1f MB total)\n", size_id, m, n, p,
           ((double)m*p + (double)p*n + (double)m*n) * sizeof(float) / (1024.0*1024.0));

    for (int draw = 0; draw < o.draws; ++draw) {
      std::mt19937 rng((unsigned)(o.baseSeed + draw));
      std::uniform_real_distribution<float> dist(-1.0f, 1.0f);

      std::vector<float> hA((size_t)m * p), hB((size_t)p * n);
      std::generate(hA.begin(), hA.end(), [&]{ return dist(rng); });
      std::generate(hB.begin(), hB.end(), [&]{ return dist(rng); });

      float *dA = nullptr, *dB = nullptr, *dC = nullptr;
      CUDA_CHECK(cudaMalloc(&dA, (size_t)m * p * sizeof(float)));
      CUDA_CHECK(cudaMalloc(&dB, (size_t)p * n * sizeof(float)));
      CUDA_CHECK(cudaMalloc(&dC, (size_t)m * n * sizeof(float)));
      CUDA_CHECK(cudaMemcpy(dA, hA.data(), (size_t)m*p*sizeof(float), cudaMemcpyHostToDevice));
      CUDA_CHECK(cudaMemcpy(dB, hB.data(), (size_t)p*n*sizeof(float), cudaMemcpyHostToDevice));

      // CPU reference (double precision, correctness only).
      std::vector<double> hCref((size_t)m * n);
      gemmRef(hA.data(), hB.data(), hCref.data(), m, n, p);

      // FLOPs: 2*m*n*p (multiply + add per output element).
      const double flops = 2.0 * m * n * p;
      // Bandwidth: read A + read B + write C.
      const double bytes = ((double)m*p + (double)p*n + (double)m*n) * sizeof(float);

      for (const KernelSpec* ks : active) {
        auto launch = ks->launch;

        // Correctness: one run, compare to CPU reference.
        launch(dC, dA, dB, m, n, p, stream);
        CUDA_CHECK(cudaGetLastError());
        CUDA_CHECK(cudaStreamSynchronize(stream));

        std::vector<float> hC((size_t)m * n);
        CUDA_CHECK(cudaMemcpy(hC.data(), dC, (size_t)m*n*sizeof(float), cudaMemcpyDeviceToHost));

        double maxDiff = 0.0, maxRef = 0.0;
        for (size_t i = 0; i < (size_t)m * n; ++i) {
            maxDiff = std::max(maxDiff, std::fabs((double)hC[i] - hCref[i]));
            maxRef  = std::max(maxRef,  std::fabs(hCref[i]));
        }
        double rel_err = (maxRef > 1e-30) ? maxDiff / maxRef : maxDiff;
        bool ok = rel_err <= o.tol;
        if (!ok) any_fail = true;
        corr << size_id << "," << ks->name << "," << m << "," << n << "," << p << ","
             << draw << "," << rel_err << "," << (ok ? 1 : 0) << "\n";
        if (!ok)
          fprintf(stderr, "  [%s] FAIL %dx%dx%d draw %d: rel_err=%e\n",
                  ks->name, m, n, p, draw, rel_err);

        // Warmup (untimed).
        for (int w = 0; w < o.warmup; ++w) {
          if (o.l2flush) flushL2(flushBuf, flushN, stream);
          launch(dC, dA, dB, m, n, p, stream);
        }
        CUDA_CHECK(cudaStreamSynchronize(stream));

        // Timed samples.
        for (int s = 0; s < o.iters; ++s) {
          if (o.l2flush) flushL2(flushBuf, flushN, stream);
          timer.begin();
          launch(dC, dA, dB, m, n, p, stream);
          float ms = timer.end();
          double us = ms * 1000.0;
          double tflops = flops / (us * 1e-6) / 1e12;
          samples << size_id << "," << ks->name << "," << m << "," << n << "," << p << ","
                  << draw << "," << s << "," << us << "," << tflops << "\n";
          total_samples++;
        }
      }

      CUDA_CHECK(cudaFree(dA));
      CUDA_CHECK(cudaFree(dB));
      CUDA_CHECK(cudaFree(dC));
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

  printf("done: %ld samples written to %s/{samples,correctness,meta}.csv\n",
         total_samples, o.outDir.c_str());
  return any_fail ? 1 : 0;
}
