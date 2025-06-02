const int N = 5000;
\
void reduction_reinit(double A[N][N], double* sum){
 *sum = 0;
 double x[1];
 x[0] = *sum;
 int nb_threads = 10;
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      x[0] += A[i][j];
      A[i][j] = 0.;
    }
  }
 *sum = x[0];
}
