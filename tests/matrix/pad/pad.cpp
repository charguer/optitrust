#include <optitrust_models.h>

/*
TODO :
- allocate different arrays of different dims
- pad by 1, pad by more than 1, pad different dimensions
- Read, write
- Negative test: try to read/write on the padded part (should not be possible)
*/

// Padding by 1, on second coordinate
int main1() {
  /* Make sure this is well matched */
  int* const a = (int*) malloc(MSIZE2(32, 32) * sizeof(int));
  // int* const b = (int*) malloc(MSIZE3(32, 32, 32) * sizeof(int));

  a[MINDEX2(32, 32, 0, 0)] = 1;

  // for (int i = 0; i < 32; i++) {
  //   for (int j = 0; j < 32; j++) {
  //     a[MINDEX2(32, 32, i, j)] = i+j;
  //   }
  // }

  // int sum = 0;

  // for (int i = 0; i < 32; i++) {
  //   for (int j = 0; j < 32; j++) {
  //     sum += a[MINDEX2(32, 32, i, j)];
  //   }
  // }

  // free(a);

  return 0;
}
/*
// Padding by 10, on first coordinate
int main2() {
  int* const a = (int*) malloc(MSIZE2(32, 32) * sizeof(int));

  for (int i = 0; i < 32; i++) {
    for (int j = 0; j < 32; j++) {
      a[MINDEX2(32, 32, i, j)] = i+j;
    }
  }

  int sum = 0;

  for (int i = 0; i < 32; i++) {
    for (int j = 0; j < 32; j++) {
      sum += a[MINDEX2(32, 32, i, j)];
    }
  }

  free(a);

  return 0;
} */
/*
int main() {
  return 0;
}
 */
/*
// Testing callocs
int main2() {
  float *const a = CALLOC1(float, 10);
  float *const b = CALLOC2(float, 10, 10);
  float *const c = CALLOC3(float, 10, 10, 10);

  for (int i = 0; i < 10; i++) {
    a[MINDEX1(10, i)] = i + 1;
    b[MINDEX2(10, 10, 5, i)] = i + 1;
    c[MINDEX3(10, 10, 10, 5, 5, i)] = i + 1;
  }
}

// Read and Write
int main3() {
  float *const a = MALLOC2(float, 10, 10);
  float *const b = MALLOC2(float, 10, 10);
  for (int i = 0; i < 10; i++) {
    a[MINDEX2(10, 10, 2, i)] = i + 1;
    b[MINDEX2(10, 10, 2, i)] = a[MINDEX2(10, 10, 2, i)];
  }
}

// Block size and nb blocks as trms
int main4() {
  int N1 = 10;
  int const block_size = 5;
  int const nb_blocks = 2;
  float *const a = MALLOC1(float, N1);

  a[MINDEX1(N1, 4)] = 42;
}

// Unknown nb_blocks
int main5() {
  int N1 = 10;
  int const block_size = 5;
  float *const a = MALLOC1(float, N1);

  a[MINDEX1(N1, 4)] = 42;
}
// block size and nb_blocks are arguments

int main6(int const N1, int const nb_blocks, int const block_size) {

  float *const a = MALLOC1(float, N1);
  for (int i = 0; i < nb_blocks; i++) {
    for (int j = 0; j < block_size; j++) {

      a[MINDEX1(N1, i * block_size + j)] = 42;
    }
  }
}
int test() {

  float *const a = CALLOC1(float, 10);
  a[MINDEX1(10, 2)] = 10;
  return 1;
}
 */
