const int N = 10000;
const int M = 1024;
float Q[N][N];
float P(const int i, const int k)
{
  return Q[i][k] * Q[k][i];
}

float accum(void)
{
  float tmp = 0.0;
  for (int i=0; i < N; i++) {
    float tmp1 = 0.0;
    for (int k=0; k < M; k++) {
      tmp1 += P(i,k);
    }
    tmp += tmp1;
  }
  return tmp;
}
