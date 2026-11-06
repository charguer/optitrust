// benchmark.cu — reduction: correctness + timed sampling driver (kernel-registry design).
//
// Pipeline, per the experimental protocol:
//   for each N:
//     for each random draw:
//       - generate a random input A (N floats)
//       - compute CPU reference sum (Kahan) — correctness only, not timed
//       - for each registered GPU kernel:
//           run once, verify sum against CPU reference (relative tolerance)
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
// Each entry describes one GPU implementation to benchmark.
// All kernels share the signature:
//   __global__ void kernel(float* partial_sums, const float* arr, int N)
//   launched as: kernel<<<N/512, 256, SMEM>>>(d_partial, d_arr, N)
struct KernelSpec {
  const char* name;
  // The launch lambda encapsulates the <<<>>> syntax so the driver
  // stays kernel-agnostic.
  std::function<void(float*, const float*, int, cudaStream_t)> launch;
};

static std::vector<KernelSpec> allKernels() {
  auto grid = [](int N) { return N / (REDUCE_TPB * REDUCE_EPT); };

  return {
    // --- OptiGPU generated ---
    {"OptiGPU",
     [grid](float* ps, const float* arr, int N, cudaStream_t s) {
       reduceOptiGPU<<<grid(N), REDUCE_TPB, REDUCE_SMEM, s>>>(ps, arr, N);
     }},

    {"OptiGPU-simpl",
      [grid](float* ps, const float* arr, int N, cudaStream_t s) {
        reduceOptiGPUSimplified<<<grid(N), REDUCE_TPB, REDUCE_SMEM, s>>>(ps, arr, N);
    }},

    // --- reference (reduce3) ---
    {"reduce3",
     [grid](float* ps, const float* arr, int N, cudaStream_t s) {
       reduce3<float><<<grid(N), REDUCE_TPB, REDUCE_SMEM, s>>>(ps, arr, N);
     }},

    // --- best performing (reduce6) ---
    {"reduce6",
     [grid](float* ps, const float* arr, int N, cudaStream_t s) {
       reduce6<float, REDUCE_TPB, true><<<grid(N), REDUCE_TPB, REDUCE_SMEM, s>>>(ps, arr, N);
     }},

    // --- best performing (reduce6) ---
    {"qwen",
     [grid](float* ps, const float* arr, int N, cudaStream_t s) {
       reduceQwen<<<grid(N), REDUCE_TPB, REDUCE_SMEM, s>>>(ps, arr, N);
     }},

    // =========================================================================
    // ADD MORE KERNELS HERE.  One entry per kernel.  Example:
    //
    // {"mykernel",
    //  [grid](float* ps, const float* arr, int N, cudaStream_t s) {
    //    myKernel<<<grid(N), REDUCE_TPB, REDUCE_SMEM, s>>>(ps, arr, N);
    //  }},
    // =========================================================================
  };
}

// ===========================================================================
// CPU reference (correctness only — not timed)
// ===========================================================================
static float reduceRef(const float* arr, int N) {
  float sum = 0.f, c = 0.f;  // Kahan summation
  for (int i = 0; i < N; ++i) {
    float y = arr[i] - c;
    float t = sum + y;
    c = (t - sum) - y;
    sum = t;
  }
  return sum;
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
  int  iters   = 50;
  int  draws   = 3;
  int  warmup  = 10;
  long baseSeed = 0;
  bool l2flush = true;
  double tol  = 1e-3;  // relative tolerance for sum comparison
  std::string outDir = "results";
  std::string impl = "all";  // "all" or comma-separated kernel names
  std::vector<Config> sizes;
  bool hasExplicitSizes = false;
};

static std::vector<Config> defaultSizes() {
  // Powers of 2, all multiples of 512 (256 threads × 2 elements).
  return {
    {262144},    // 2^18  =  256K  (  1 MB)
    {1048576},   // 2^20  =    1M  (  4 MB)
    {4194304},   // 2^22  =    4M  ( 16 MB)
    {16777216},  // 2^24  =   16M  ( 64 MB)
    {67108864},  // 2^26  =   64M  (256 MB)
    {268435456}, // 2^28  =  256M  (  1 GB)
    {1073741824} // 2^30  =    1G  (  4 GB)
  };
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
        int N = std::stoi(tok);
        if (N % (REDUCE_TPB * REDUCE_EPT) != 0) {
          fprintf(stderr, "N=%d not a multiple of %d\n",
                  N, REDUCE_TPB * REDUCE_EPT);
          exit(2);
        }
        o.sizes.push_back({N});
        if (c == std::string::npos) break;
        start = c + 1;
      }
    }
    else if (a == "-h" || a == "--help") {
      fprintf(stderr,
        "options: --iters N --draws N --warmup N --seed S --tol F "
        "--out DIR --impl NAMES --no-l2flush --sizes 'N,N,...'\n"
        "  --impl: 'all' (default) or comma-separated kernel names\n"
        "  N must be a multiple of 512.\n");
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

  if (::mkdir(o.outDir.c_str(), 0755) != 0 && errno != EEXIST) {
    fprintf(stderr, "cannot create output dir '%s': %s\n",
            o.outDir.c_str(), strerror(errno));
    return 2;
  }
  std::ofstream samples(o.outDir + "/samples.csv", std::ios::trunc);
  std::ofstream corr(o.outDir + "/correctness.csv", std::ios::trunc);
  std::ofstream meta(o.outDir + "/meta.csv", std::ios::trunc);
  samples << "size_id,impl,N,draw,sample,time_us,gbps\n";
  corr    << "size_id,impl,N,draw,rel_err,ok\n";

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
       << "threads_per_block," << REDUCE_TPB << "\n"
       << "elements_per_thread," << REDUCE_EPT << "\n"
       << "shared_mem_bytes," << REDUCE_SMEM << "\n"
       << "iters," << o.iters << "\n"
       << "draws," << o.draws << "\n"
       << "warmup," << o.warmup << "\n"
       << "l2_flush," << (o.l2flush ? 1 : 0) << "\n"
       << "tol," << o.tol << "\n";

  int size_id = 0;
  long total_samples = 0;
  bool any_fail = false;

  for (const Config& cfg : o.sizes) {
    const int N = cfg.N;
    const int num_blocks = N / (REDUCE_TPB * REDUCE_EPT);
    printf("[size %02d] N=%d  (%.1f MB)\n", size_id, N,
           (double)N * sizeof(float) / (1024.0 * 1024.0));

    for (int draw = 0; draw < o.draws; ++draw) {
      std::mt19937 rng((unsigned)(o.baseSeed + draw));
      std::uniform_real_distribution<float> dist(-1.0f, 1.0f);

      std::vector<float> hA(N);
      std::generate(hA.begin(), hA.end(), [&] { return dist(rng); });

      float *dA = nullptr, *dPartial = nullptr;
      CUDA_CHECK(cudaMalloc(&dA, N * sizeof(float)));
      CUDA_CHECK(cudaMalloc(&dPartial, num_blocks * sizeof(float)));
      CUDA_CHECK(cudaMemcpy(dA, hA.data(), N * sizeof(float),
                            cudaMemcpyHostToDevice));

      // CPU reference sum (correctness only).
      float ref_sum = reduceRef(hA.data(), N);

      // Bandwidth: read N floats + write num_blocks partial sums.
      const double bytes = (N + num_blocks) * sizeof(float);

      // ---- iterate over all active kernels ----
      for (const KernelSpec* ks : active) {
        auto launch = ks->launch;

        // Correctness: one run, compare final sum to CPU reference.
        launch(dPartial, dA, N, stream);
        CUDA_CHECK(cudaGetLastError());
        CUDA_CHECK(cudaStreamSynchronize(stream));

        std::vector<float> hPartial(num_blocks);
        CUDA_CHECK(cudaMemcpy(hPartial.data(), dPartial,
                              num_blocks * sizeof(float),
                              cudaMemcpyDeviceToHost));
        float gpu_sum = 0.f;
        for (int b = 0; b < num_blocks; ++b) gpu_sum += hPartial[b];

        double rel_err = 0.0;
        if (std::fabs(ref_sum) > 1e-30)
          rel_err = std::fabs((double)gpu_sum - (double)ref_sum)
                    / std::fabs((double)ref_sum);
        bool ok = rel_err <= o.tol;
        if (!ok) any_fail = true;
        corr << size_id << "," << ks->name << "," << N << ","
             << draw << "," << rel_err << "," << (ok ? 1 : 0) << "\n";
        if (!ok)
          fprintf(stderr, "  [%s] FAIL N=%d draw %d: rel_err=%e\n",
                  ks->name, size_id, draw, rel_err);

        // Warmup (untimed).
        for (int w = 0; w < o.warmup; ++w) {
          if (o.l2flush) flushL2(flushBuf, flushN, stream);
          launch(dPartial, dA, N, stream);
        }
        CUDA_CHECK(cudaStreamSynchronize(stream));

        // Timed samples.
        for (int s = 0; s < o.iters; ++s) {
          if (o.l2flush) flushL2(flushBuf, flushN, stream);
          timer.begin();
          launch(dPartial, dA, N, stream);
          float ms = timer.end();
          double us = ms * 1000.0;
          double gbps = bytes / (us * 1e-6) / 1e9;
          samples << size_id << "," << ks->name << "," << N << ","
                  << draw << "," << s << "," << us << "," << gbps << "\n";
          total_samples++;
        }
      }

      CUDA_CHECK(cudaFree(dA));
      CUDA_CHECK(cudaFree(dPartial));
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
