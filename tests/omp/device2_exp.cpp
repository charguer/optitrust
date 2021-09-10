#include <omp.h>

void init(float *, float *, int);

void output(float *, int);

void vec_mult(float *p, float *v1, float *v2, int N) {
  init(v1, v2, N);
  int ndev = omp_get_num_devices();
  int do_offload = ((ndev > 0) && (N > 1000000));
#pragma omp target if (do_offload),                                            \
    map(to                                                                     \
        : v1 [0:N], v2[:N]),                                                   \
    map(from                                                                   \
        : p [0:N])
#pragma omp parallel for if (N > 10000) private(i)
  for (int i = 0; (i < N); i++)
    p[i] = (v1[i] * v2[i]);
  output(p, N);
}