void compute_array(float *node, int M);

void compute_matrix(float *array, int N, int M) {
#pragma omp parallel private(i)
#pragma omp single
  {
    for (int i = 0; (i < N); i++) {
#pragma omp task priority(i)
      compute_array((&array[(i * M)]), M);
    }
  }
}