#include <optitrust_models.h>

/*
TODO :
- allocate different arrays of different dims
- pad by 1, pad by more than 1, pad different dimensions (ok)
- Read, write (ok)
- Negative test: try to read/write on the padded part (should not be possible)
*/

// Padding by 1, on second coordinate
int main1() {
  /* Make sure this is well matched */
  int* const a = (int*) malloc(MSIZE2(32, 32) * sizeof(int));
  int* const b = (int*) malloc(MSIZE3(32, 32, 32) * sizeof(int));

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
}


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
}

int main3() {
  /* Make sure this is well matched */
  float* const a = (float*) malloc(MSIZE3(16, 32, 8) * sizeof(float));
  int* const b = (int*) malloc(MSIZE4(64, 16, 32, 8) * sizeof(int));

  for (int i = 0; i < 16; i++) {
    for (int j = 0; j < 32; j++) {
      for (int k = 0; k < 8; k++) {
        a[MINDEX3(16, 32, 8, i, j, k)] = (float) i+j+k;
      }
    }
  }

  int sum = 0;

  for (int i = 0; i < 64; i++) {
    for (int j = 0; j < 16; j++) {
      for (int k = 0; k < 32; k++) {
        for (int l = 0; l < 8; l++) {
        b[MINDEX4(64, 16, 32, 8, i, j, k, l)] = (i+j+k)*l;
        }
      }
    }
  }

  free(a);
  free(b);

  return 0;
}
