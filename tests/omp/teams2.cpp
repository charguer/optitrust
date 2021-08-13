
int min(int x, int y) {
  return x < y ? x : y;
}

float dotprod(float B[], float C[], int N, int block_size,
  int num_teams, int block_threads)
{
    float sum = 0.0;
    for (int i0=0; i0<N; i0 += block_size){
       for (int i=i0; i< min(i0+block_size,N); i++)
           sum += B[i] * C[i];
    }
    return sum;
}
