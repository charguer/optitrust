void matmul_depend(int N, int BS, float A[N][N], float B[N][N], float C[N][N] )
{
   for (int i = 0; i < N; i+=BS) {
      for (int j = 0; j < N; j+=BS) {
         for (int k = 0; k < N; k+=BS) {
   // Note 1: i, j, k, A, B, C are firstprivate by default
   // Note 2: A, B and C are just pointers
            for (int ii = i; ii < i+BS; ii++ )
               for (int jj = j; jj < j+BS; jj++ )
                  for (int kk = k; kk < k+BS; kk++ )
                     C[ii][jj] = C[ii][jj] + A[ii][kk] * B[kk][jj];
         }
      }
   }
}