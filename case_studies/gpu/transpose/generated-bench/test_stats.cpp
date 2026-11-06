#include "../stats.h"

#include <cmath>
#include <cstdio>
#include <vector>

namespace {

int failures = 0;

void expect_near(double got, double want, double eps, const char* what) {
  if (std::fabs(got - want) > eps) {
    std::fprintf(stderr, "FAIL %s: got %.12g want %.12g\n", what, got, want);
    ++failures;
  }
}

}  // namespace

int main() {
  {
    std::vector<double> v;
    for (int i = 1; i <= 10; ++i) v.push_back((double)i);
    const SummaryStats s = summarize(v);
    expect_near((double)s.n, 10.0, 0.0, "n");
    expect_near(s.mean, 5.5, 1e-12, "mean");
    expect_near(s.stddev, std::sqrt(82.5 / 9.0), 1e-12, "stddev");
    expect_near(s.min, 1.0, 0.0, "min");
    expect_near(s.max, 10.0, 0.0, "max");
    expect_near(s.q1, 3.25, 1e-12, "q1");
    expect_near(s.median, 5.5, 1e-12, "median");
    expect_near(s.q3, 7.75, 1e-12, "q3");
  }
  {
    const SummaryStats s = summarize({});
    expect_near((double)s.n, 0.0, 0.0, "empty n");
  }
  {
    const SummaryStats s = summarize({42.0});
    expect_near(s.median, 42.0, 0.0, "single median");
    expect_near(s.stddev, 0.0, 0.0, "single stddev");
  }
  {
    std::vector<double> v{5.0, 1.0, 3.0, 2.0, 4.0};
    const SummaryStats s = summarize(v);
    expect_near(s.median, 3.0, 1e-12, "unsorted median");
    expect_near(s.q1, 2.0, 1e-12, "unsorted q1");
    expect_near(s.q3, 4.0, 1e-12, "unsorted q3");
  }
  if (failures == 0) {
    std::printf("all stats tests passed\n");
    return 0;
  }
  std::fprintf(stderr, "%d test(s) failed\n", failures);
  return 1;
}
