#include <optitrust.h>

int main() {
  const int a_blocks = (10 + (2 - 1)) / 2;
  float* const a = (float*)malloc(MSIZE2(a_blocks, 2) * sizeof(float));
  const int b_blocks = (10 + (2 - 1)) / 2;
  float* const b = (float*)malloc(MSIZE3(b_blocks, 2, 10) * sizeof(float));
  const int c_blocks = (10 + (2 - 1)) / 2;
  float* const c = (float*)malloc(MSIZE4(10, 10, c_blocks, 2) * sizeof(float));
  for (int i = 0; i < 10; i++) {
    a[MINDEX2(a_blocks, 2, i / 2, i % 2)] = i + 1;
    b[MINDEX3(b_blocks, 2, 10, 5 / 2, 5 % 2, i)] = i + 1;
    c[MINDEX4(10, 10, c_blocks, 2, 5, 5, i / 2, i % 2)] = i + 1;
  }
}

int main2() {
  const int a_blocks = (10 + (2 - 1)) / 2;
  float* const a = (float*)calloc(MSIZE2(a_blocks, 2), sizeof(float));
  const int b_blocks = (10 + (2 - 1)) / 2;
  float* const b = (float*)calloc(MSIZE3(b_blocks, 2, 10), sizeof(float));
  const int c_blocks = (10 + (2 - 1)) / 2;
  float* const c = (float*)calloc(MSIZE4(c_blocks, 2, 10, 10), sizeof(float));
  for (int i = 0; i < 10; i++) {
    a[MINDEX2(a_blocks, 2, i / 2, i % 2)] = i + 1;
    b[MINDEX3(b_blocks, 2, 10, 5 / 2, 5 % 2, i)] = i + 1;
    c[MINDEX4(c_blocks, 2, 10, 10, 5 / 2, 5 % 2, 5, i)] = i + 1;
  }
}

int main3() {
  const int a_blocks = (10 + (2 - 1)) / 2;
  float* const a = (float*)malloc(MSIZE3(a_blocks, 2, 10) * sizeof(float));
  float* const b = (float*)malloc(MSIZE2(10, 10) * sizeof(float));
  for (int i = 0; i < 10; i++) {
    a[MINDEX3(a_blocks, 2, 10, 2 / 2, 2 % 2, i)] = i + 1;
    b[MINDEX2(10, 10, 2, i)] = a[MINDEX3(a_blocks, 2, 10, 2 / 2, 2 % 2, i)];
  }
}

int main4() {
  int N1 = 10;
  const int block_size = 5;
  const int nb_blocks = 2;
  float* const a =
      (float*)malloc(MSIZE2(nb_blocks, block_size) * sizeof(float));
  a[MINDEX2(nb_blocks, block_size, 4 / block_size, 4 % block_size)] = 42;
}

int main5() {
  int N1 = 10;
  const int block_size = 5;
  const int a_blocks = (N1 + (block_size - 1)) / block_size;
  float* const a = (float*)malloc(MSIZE2(a_blocks, block_size) * sizeof(float));
  a[MINDEX2(a_blocks, block_size, 4 / block_size, 4 % block_size)] = 42;
}

int test() {
  const int a_blocks = (10 + (2 - 1)) / 2;
  float* const a = (float*)calloc(MSIZE2(a_blocks, 2), sizeof(float));
  a[MINDEX2(a_blocks, 2, 2 / 2, 2 % 2)] = 10;
  return 1;
}
