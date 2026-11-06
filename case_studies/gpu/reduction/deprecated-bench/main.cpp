#include <stdlib.h>
#include <stdio.h>
#include <math.h>

float reduce(float* arr, int N, int whichKernel);

int main(int argc, char **argv)
{
    const int N = 1 << 24;
    const size_t mem_size = N * sizeof(float);

    float *idata = (float*)malloc(mem_size);

    for (int i = 0; i < N; ++i) {
        idata[i] = (float)i;
    }

    float results[3];
    int fail = 0;
    for (int i = 0; i < 3; i++) {
      results[i] = reduce(idata, N, i);
      if (results[i] !=  results[0]) {
        fail = 1;
      }
    }
    printf("OptiGPU: %f \nreference: %f \nbest: %f\n", results[0], results[1], results[2]);
    if (fail) {
      fprintf(stderr, "failed equivalence check");
      exit(EXIT_FAILURE);
    }

    printf("success\n");

    free(idata);
    return 0;
}
