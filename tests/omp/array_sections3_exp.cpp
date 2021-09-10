void foo() {
  int[30] A, p;
#pragma omp target data[map(A [0:4])]
  {
    p = (&A[0]);
#pragma omp target map(p [7:20])
    {
      A[2] = 0;
      p[8] = 0;
    }
  }
}