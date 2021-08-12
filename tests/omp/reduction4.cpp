#include <stdio.h>

const int N = 100;
void init(int n, float (*b)[N]);

int main(){

  int j;
  float a[N], b[N][N];

  init(N,b);

  for(int k = 0; k <N; k++) a[k]=0.0e0;

  for(int i=0; i<N; i++){
    for(j=0; j<N; j++){
       a[j] +=  b[i][j];
    }
  }
  printf(" a[0] a[N-1]: %f %f\n", a[0], a[N-1]);

  return 0;
}


