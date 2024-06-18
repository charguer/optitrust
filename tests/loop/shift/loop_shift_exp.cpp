int main() {
  int x = 0;
  const int st = 0;
  const int N = 10;
  for (int i_s = 4; i_s < 12; i_s++) {
    x += i_s;
  }
  for (int i2 = 2; i2 < 12; i2++) {
    x += i2 - 2;
  }
  int w = 10 + 2;
  for (int j2 = 0; j2 < N; j2++) {
    x += j2 + st;
  }
  int shift = 5;
  for (int k2 = shift; k2 < N + shift; k2++) {
    const int k = k2 - shift;
    x += k;
  }
  for (int l = N + shift; l > shift; l--) {
    x += l - shift;
  }
  for (int m = 8; m < N + 4; m++) {
    x += m - 6;
  }
  float* input;
  float* output;
  for (int bi = 0; bi < N; bi += 32) {
    for (int i = 0; i < 32; i++) {
      float sum = (float)0.;
      for (int k2 = 0; k2 < N; k2++) {
        sum += input[k2 + N * (i + bi)];
      }
      output[N * (i + bi)] = sum;
    }
  }
}
