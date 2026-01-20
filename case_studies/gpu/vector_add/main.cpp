#include <stdlib.h>
#include <stdio.h>
#include <math.h>

void vector_add(float* a, float* b, float* c, int N);

int main() {
  int    numElements = 256 * 512;
  size_t size        = numElements * sizeof(float);

   // Allocate the host input vector A
  float *h_A = (float *)malloc(size);

  // Allocate the host input vector B
  float *h_B = (float *)malloc(size);

  // Allocate the host output vector C
  float *h_C = (float *)malloc(size);

  // Initialize the host input vectors
  for (int i = 0; i < numElements; ++i) {
      h_A[i] = rand() / (float)RAND_MAX;
      h_B[i] = rand() / (float)RAND_MAX;
  }

  vector_add(h_A, h_B, h_C, numElements);

  // Verify that the result vector is correct
  for (int i = 0; i < numElements; ++i) {
      if (fabs(h_A[i] + h_B[i] - h_C[i]) > 1e-5) {
          fprintf(stderr, "Result verification failed at element %d!\n", i);
          exit(EXIT_FAILURE);
      }
  }

  printf("success\n");

  free(h_A);
  free(h_B);
  free(h_C);
}
