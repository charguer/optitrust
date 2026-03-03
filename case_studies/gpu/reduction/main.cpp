#include <stdlib.h>
#include <stdio.h>
#include <math.h>

float reduce(float* arr, int N);

float reduce_gold(float *arr, int N) {
  float sum = 0;
  for (int i = 0; i < N; i++) {
    sum += arr[i];
  }
  return sum;
}

int main(int argc, char **argv)
{
    const int N = 1 << 24;
    const size_t mem_size = N * sizeof(float);

    float *idata = (float*)malloc(mem_size);

    for (int i = 0; i < N; ++i) {
        idata[i] = (float)i;
    }

    float ref = reduce_gold(idata, N);
    float ours = reduce(idata, N);

    if (fabs(ref - ours) > (1e-5 * ((float)N))) {
      fprintf(stderr, "failed: ref %f vs %f \n", ref, ours);
      exit(EXIT_FAILURE);
    }

    printf("success\n");

    free(idata);
    return 0;
}
