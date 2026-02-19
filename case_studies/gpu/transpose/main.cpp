#include <stdlib.h>
#include <stdio.h>
#include <math.h>


void transpose(float* a, float* b, int W, int H);

void computeTransposeGold(float *gold, float *idata, const int size_x, const int size_y)
{
    for (int y = 0; y < size_y; ++y) {
        for (int x = 0; x < size_x; ++x) {
            gold[(x * size_y) + y] = idata[(y * size_x) + x];
        }
    }
}


int main(int argc, char **argv)
{
    const int W = 1024;
    const int H = 1024;
    const size_t mem_size = W * H * sizeof(float);

    float *idata = (float*)malloc(mem_size);
    float *odata = (float*)malloc(mem_size);
    float *gold = (float*)malloc(mem_size);

    for (int i = 0; i < (W*H); ++i) {
        idata[i] = (float)i;
    }

    computeTransposeGold(gold, idata, W, H);
    transpose(idata, odata, W, H);

    for (int i = 0; i < (W*H); ++i) {
      if (fabs(odata[i] - gold[i]) > 1e-5) {
          fprintf(stderr, "Result verification failed at element %d (%f gold vs %f)!\n", i, gold[i], odata[i]);
          exit(EXIT_FAILURE);
      }
    }

    printf("success\n");

    free(idata);
    return 0;
}
