#pragma once

#include <cstdlib>
#include <cstdio>
#include <cmath>
#include <vector>
#include <algorithm>

#include "HalideBuffer.h"
#include "HalideRuntime.h"
#include "halide_benchmark.h"

// TODO: put some of this code in a .cpp?

// Attempt to use at least this much time (in seconds) for one sample;
// initial iterations will be done to find an iterations-per-sample
// count that puts the sample runtime in this ballpark.
constexpr double MIN_TIME = 0.05;

// Upper time limit.
constexpr double MAX_TIME = 5.0;

// Maximum value for the computed iters-per-sample.
constexpr uint32_t MAX_ITERS_PER_SAMPLE = 100000;

constexpr uint32_t MIN_SAMPLES = 3;

// Terminate when the relative difference between the lower quartile
// and the upper quartile of the runtimes is no more than this.
// Controls accuracy. The closer to zero this gets the more
// reliable the answer, but the longer it may take to run.
constexpr double ACCURACY = 0.01;

inline double benchmark_sample(uint32_t iterations, const std::function<void(uint32_t)> &f) {
  auto start = Halide::Tools::benchmark_now();
  for (uint32_t i = 0; i < iterations; i++) {
    f(i);
  }
  auto end = Halide::Tools::benchmark_now();
  double elapsed_seconds = Halide::Tools::benchmark_duration_seconds(start, end);
  return elapsed_seconds;
}

// returns the amount of iterations performed,
// including the ones used to infer iters_per_sample:
inline uint32_t benchmark_and_print_samples(const std::function<void(uint32_t)> &f) {
  std::vector<double> samples;
  // includes time and iterations used to infer iters_per_sample:
  double total_time = 0.0;
  uint32_t total_iterations = 0;

  // Any time result <= to this is considered 'zero' here.
  constexpr double kTimeEpsilon = 1e-9;
  static_assert(MIN_TIME > kTimeEpsilon);
  static_assert(MIN_TIME < MAX_TIME);

  uint32_t iters_per_sample = 1;
  while (true) {
    samples.clear();
    for (uint32_t s = 0; s < MIN_SAMPLES; s++) {
      samples.push_back(benchmark_sample(iters_per_sample, [&](uint32_t i) {
        f(total_iterations + i);
      }));
      total_time += samples[s];
      total_iterations += iters_per_sample;
    }
    std::sort(samples.begin(), samples.end());

    fprintf(stderr, "iterations per sample: %u, sample time: %f\n",
      iters_per_sample, samples[0]);
    if (samples[0] < kTimeEpsilon) {
      // If the fastest time is tiny, then trying to use it to predict next_iters
      // can just explode into something unpredictably huge, which could take far too
      // long to complete. Just double iters_per_sample and try again (or terminate if
      // we're over the max).
      iters_per_sample *= 2;
    } else {
      if (samples[0] >= MIN_TIME) {
        fprintf(stderr, "iterations per sample fixed, sample time above minimum\n");
        // note: we keep the collected samples
        break;
      }
      // Use an estimate based on initial times to converge faster.
      const double next_iters = std::max(MIN_TIME / samples[0],
                                         iters_per_sample * 2.0);
      iters_per_sample = (uint32_t)(next_iters + 0.5);
    }

    // Ensure we never explode beyond the max.
    if (iters_per_sample >= MAX_ITERS_PER_SAMPLE) {
      iters_per_sample = MAX_ITERS_PER_SAMPLE;
      samples.clear(); // invalidate the collected samples
      fprintf(stderr, "iterations per sample capped to %u\n", iters_per_sample);
      break;
    }
  }

  // Note: changes samples to be divided by number of iterations,
  // reflecting the runtime of a single function call.
  for (double& sample : samples) {
    sample /= (double)iters_per_sample;
  }

  // - Keep taking samples until we are accurate enough (even if we run over min_time).
  // - If we are already accurate enough but have time remaining, keep taking samples.
  // - No matter what, don't go over max_time; this is important, in case
  // we happen to get faster results for the first samples, then happen to transition
  // to throttled-down CPU state.
  const double accuracy_scaling = 1.0 + ACCURACY;
  while (((samples[std::floor(samples.size()*0.25)] * accuracy_scaling <
           samples[std::floor(samples.size()*0.75)])
           || total_time < MIN_TIME) &&
          total_time < MAX_TIME) {
      double sample = benchmark_sample(iters_per_sample, [&](uint32_t i) {
        f(total_iterations + i);
      });
      total_time += sample;
      total_iterations += iters_per_sample;
      samples.push_back(sample / (double)iters_per_sample);
      std::sort(samples.begin(), samples.end());
  }

  for (double& sample : samples) {
    printf("sample: %lf\n", sample);
  }
  std::vector<double> quartiles = {
    0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99
  };
  for (double& q : quartiles) {
    printf("%uth percentile: %lf\n", (uint8_t)(q*100.0), samples[std::floor(samples.size()*q)]);
  }
  fprintf(stderr, "took %zu samples of %u iterations\n",
    samples.size(), iters_per_sample);

  return total_iterations;
}

void error_stats(float* gold, float* other, size_t n, double tolerated_per_pixel, double required_psnr) {
  double square_sum = 0.;
  double min = 1. / 0.;
  double max = 0.;
  float min_g = 1.f / 0.f;
  float max_g = -min_g;

  for (size_t i = 0; i < n; i++) {
    min_g = std::min(min_g, gold[i]);
    max_g = std::max(max_g, gold[i]);
    double delta = (double)(gold[i]) - (double)(other[i]);
    double d_abs = std::abs(delta);
    min = std::min(min, d_abs);
    max = std::max(max, d_abs);
    square_sum += d_abs * d_abs;
  }

  double mse = square_sum / n;
  double range = (double)(max_g) - (double)(min_g);
  double psnr = 10.0 * log10((range * range) / mse);
  min /= range;
  max /= range;

  fprintf(stderr, "error stats: [%.5lf - %.5lf]*%.5lf with %.2lf PSNR\n", min, max, range, psnr);
  if (max > tolerated_per_pixel || psnr < required_psnr) {
    fprintf(stderr, "maximum tolerated error of %.4f per pixel, and minimum PSNR of %.2f\n",
        tolerated_per_pixel, required_psnr);
    exit(EXIT_FAILURE);
  }
}