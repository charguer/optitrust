void compute_array (float *node, int M);

void compute_matrix (float *array, int N, int M)
{
  for (int i=0; i < N; i++) {
     compute_array(&array[i*M], M);
  }
}
