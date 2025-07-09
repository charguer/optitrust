#include <optitrust.h>
// Testing mallocs
int main() {
  float * const a = MALLOC1(float, 10);
  float * const b = MALLOC2(float, 10, 10);
  float * const c = MALLOC3(float, 10, 10, 10);
  // float * const d = MALLOC4(float, 10, 10, 10, 10);

  for (int i = 0; i < 10; i++) {
    a[MINDEX1(10, i)] = i + 1;
    b[MINDEX2(10, 10, 5, i)] = i + 1;
    c[MINDEX3(10, 10, 10, 5, 5, i)] = i + 1;
    // d[MINDEX4(10, 10, 10, 10, 5, 5, 5, i)] = i + 1;
  }
}
// Testing callocs
int main2() {
  float * const a = CALLOC1(float, 10);
  float * const b = CALLOC2(float, 10, 10);
  float * const c = CALLOC3(float, 10, 10, 10);
  // float * const d = CALLOC4(float, 10, 10, 10, 10);

  for (int i = 0; i < 10; i++) {
    a[MINDEX1(10, i)] = i + 1;
    b[MINDEX2(10, 10, 5, i)] = i + 1;
    c[MINDEX3(10, 10, 10, 5, 5, i)] = i + 1;
    // d[MINDEX4(10, 10, 10, 10, 5, 5, 5, i)] = i + 1;
  }
}

// Read and Write
int main3() {
  float * const a = MALLOC2(float, 10, 10);
  float * const b = MALLOC2(float, 10, 10);
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
  float * const a = MALLOC1(float, N1);

  a[MINDEX1(N1, 4)] = 42;
}

// Unknown nb_blocks
int main5() {
  int N1 = 10;
  int const block_size = 5;
  float * const a = MALLOC1(float, N1);

  a[MINDEX1(N1, 4)] = 42;
}
int test() {

  float * const a = CALLOC1(float, 10);
  a[MINDEX1(10,2)] = 10;
  return 1;

}
