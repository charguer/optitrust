#include <omp.h>

int test() {
  int x = 0;
#pragma omp parallel for collapse(3)
  for (int a = 0; a < 4; a++) {
    for (int b = 0; b < 4; b++) {
      x++;
    }
  }
  x++;
#pragma omp parallel for collapse(3)
  for (int a = 0; a < 4; a++) {
    for (int b = 0; b < 4; b++) {
      x++;
    }
  }
}

void subdomain(float* x, int istart, int ipoints) {
  int i;
  for (i = 0; i < ipoints; i++) x[istart + i] = 123.456;
}

void sub(float* x, int npoints) {
  int iam, nt, ipoints, istart;
  {
    iam = omp_get_thread_num();
    nt = omp_get_num_threads();
    ipoints = npoints / nt;
    istart = iam * ipoints;
    if (iam == nt - 1) ipoints = npoints - istart;
    subdomain(x, istart, ipoints);
  }
}

int main() {
  float array[10000];
  sub(array, 10000);
  return 0;
}
