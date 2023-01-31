
#include <stdlib.h>

// output: m.n.f
//      a: m.o.f
//      b: o.n.f
void mm(float* output, float* a, float* b, int m, int n, int o) {
// output -> array<float>
// a ->^R array<float>
// b ->^R array<float>
  for (int i = 0; i < m; i++) {
    for (int j = 0; j < n; j++) {
      float sum = 0.0f;
      // sum -> cell<float>
      for (int k = 0; k < o; k++) {
        sum += a[k + (o * i)] * b[j + (n * k)];
      }

      output[(j + (n * i))] = sum;
    }
  }
}

int main() {
  const int M = 1024;
  const int N = 1024;
  const int O = 1024;
  
  float* output = (float*) calloc(M * N, sizeof(float));
  float* a = (float*) malloc(M * O * sizeof(float));
  float* b = (float*) malloc(O * N * sizeof(float));

  // TODO: init?

  mm(output, a, b, M, N, O);

  // TODO: check result?

  free(output);
  free(a);
  free(b);

  return 0;
}