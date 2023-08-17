#include "../../include/optitrust.h"

static int default_floyd(uint32_t N, uint32_t *A)
{
  // Floyd-Warshall Algorithm
  for (uint32_t k = 0; k < N; k++)
  {
    for (uint32_t i = 0; i < N; i++)
    {
      for (uint32_t j = 0; j < N; j++)
      {
        if (A[i * N + j] > A[i * N + k] + A[k * N + j])
        {
          A[i * N + j] = A[i * N + k] + A[k * N + j];
        }
      }
    }
  }
  return 0;
}
