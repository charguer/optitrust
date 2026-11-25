#include <optitrust.h>

int f(int N1, int N2, int N3, int i1, int i2, int i3) {
  const int block_size = 10;
  const int a_blocks = (N2 + (block_size - 1)) / block_size;
  float* const a =
      (float*)malloc(MSIZE4(N1, a_blocks, block_size, N3) * sizeof(float));
  a[MINDEX4(N1, a_blocks, block_size, N3, i1, i2 / block_size, i2 % block_size,
            i3)] = 0;
}
