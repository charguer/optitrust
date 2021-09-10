void foo() {
  int[30] A, p;
#pragma omp target data[map(A [0:10])]
  {
    p = (&A[0]);
#pragma omp target map(p [3:7])
    {
      A[2] = 0;
      p[8] = 0;
      A[8] = 1;
    }
  }
}