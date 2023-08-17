#include "../../include/optitrust.h"

// NOTE: using pretty matrix notation
int default_floyd(uint32_t N, uint32_t* A) {
  for (int k = 0; k < N; k++) {
    uint32_t* kj = (uint32_t*)malloc(sizeof(uint32_t[N]));
    for (int j = 0; j < N; j++) {
      kj[j] = A[k * N + j];
    }
    for (int i = 0; i < N; i++) {
      for (int j = 0; j < N; j++) {
        uint32_t sum = A[i * N + k] + kj[j];
        if (A[i * N + j] > sum) {
          A[i * N + j] = sum;
        }
      }
    }
    free(kj);
  }
  return 0;
}
