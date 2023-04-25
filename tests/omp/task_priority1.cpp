void compute_array (float *node, int M);

void compute_matrix (float *array, int N, int M)
{
  int i;
  for (i = 0; i < N; i++) {
     compute_array(&array[i*M], M);
  }
}
