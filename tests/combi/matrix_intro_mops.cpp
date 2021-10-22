#include <stdlib.h>
int main () {

  const int N = 5;
  int* p = (int*) calloc (N * sizeof(int));
  for (int i = 0; i < 5; i++){
    p[i] = i;
  }
  int* q = (int*) malloc (N * sizeof(int));
  for (int i = 0; i < 5; i++){
    q[i] = i;
  }

  return 0;
}