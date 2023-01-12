
// output: m.n.f
//      a: m.o.f
//      b: o.n.f
int mm(float* output, float* a, float* b, int m, int n, int o) {
  for (int i = 0; i < m; i++) {
    for (int j = 0; j < n; j++) {
      float sum = 0.0f;
      for (int k = 0; k < o; k++) {
        sum += a[k + (o * i)] * b[j + (n * k)];
      }

      output[(j + (n * i))] = sum;
    }
  }
}

int reduction(float* output, float* input, int n, int m) {
  for (int i = 0; i < n; i++) {
    float sum = 0.0f;
    for (int j = 0; j < m; j++) {
      sum += input[j + (i * m)];
    }
    *output = sum;
  }

  /* target:

  loop hoist;
  loop fission;
  loop swap;

  float sum[n];
  for (int i = 0; i < n; i++) {
    sum[i] = 0.0f;
  }
  for (int j = 0; j < m; j++) {
    for (int i = 0; i < n; i++) {
      sum[i] += input[j + (i * m)];
    }
  }
  for (int i = 0; i < n; i++) {
    *output = sum[i];
  }
  */
}

int main() {
  /*
  for (int i = 0; i < 5; i++) {
    for (int j = 0; j < 10; j++) {
      for (int k = 0; k < 20; k++) {

      }
    }
  }
  */
  return 0;
}