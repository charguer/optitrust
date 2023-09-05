#include "../../include/optitrust.h"

// NOTE: using pretty matrix notation
int optitrust_floyd(uint32_t N, uint32_t* A) {
  uint32_t* const kj = (uint32_t* const)malloc(sizeof(uint32_t[N]));
  uint32_t* const ik = (uint32_t* const)malloc(sizeof(uint32_t[N]));
  for (int k = 0; k < N; k++) {
    for (int j = 0; j < N; j++) {
      kj[j] = A[k * N + j];
    }
    for (int i = 0; i < N; i++) {
      ik[i] = A[i * N + k];
    }
    for (int i = 0; i < k; i++) {
      for (int j = 0; j < k; j++) {
        uint32_t sum = ik[i] + kj[j];
        if (A[i * N + j] > sum) {
          A[i * N + j] = sum;
        }
      }
    }
    for (int i = 0; i < k; i++) {
      uint32_t sum = ik[i] + kj[k];
      if (A[i * N + k] > sum) {
        A[i * N + k] = sum;
        ik[i] = sum;
      }
    }
    for (int i = 0; i < k; i++) {
      for (int j = k + 1; j < N; j++) {
        uint32_t sum = ik[i] + kj[j];
        if (A[i * N + j] > sum) {
          A[i * N + j] = sum;
        }
      }
    }
    for (int j = 0; j < N; j++) {
      uint32_t sum = ik[k] + kj[j];
      if (A[k * N + j] > sum) {
        A[k * N + j] = sum;
        kj[j] = sum;
      }
    }
    for (int i = k + 1; i < N; i++) {
      for (int j = 0; j < k; j++) {
        uint32_t sum = ik[i] + kj[j];
        if (A[i * N + j] > sum) {
          A[i * N + j] = sum;
        }
      }
    }
    for (int i = k + 1; i < N; i++) {
      uint32_t sum = ik[i] + kj[k];
      if (A[i * N + k] > sum) {
        A[i * N + k] = sum;
        ik[i] = sum;
      }
    }
    for (int i = k + 1; i < N; i++) {
      for (int j = k + 1; j < N; j++) {
        uint32_t sum = ik[i] + kj[j];
        if (A[i * N + j] > sum) {
          A[i * N + j] = sum;
        }
      }
    }
  }
  free(ik);
  free(kj);
  return 0;
}
