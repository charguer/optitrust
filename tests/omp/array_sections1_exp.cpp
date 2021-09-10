void foo() {
  int A[30];
#pragma omp target data[map(A [0:4])]
  {
#pragma omp target map(A [7:20])
    { A[2] = 0; }
  }
}