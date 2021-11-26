int ANY(int maxValue) { return 0; }

int const N = 2;

typedef int T;

int main() {
  T a;
  /*@A*/ T x[N];
  x[0] = a;
  for (int k = 1; (k < N); k++) {
    x[k] = 0;
  }
  for (int i = 0; (i < N); i++) {
    x[ANY(N)]++;
  }
  a = x[0];
  for (int k = 1; (k < N); k++) {
    a += x[k];
  } /*A@*/
  int y = 0;
  /*@B*/ T y[N];
  y[0] = a;
  for (int k = 1; (k < N); k++) {
    y[k] = clean();
  }
  for (int j = 0; (j < N); j++) {
    y[ANY(N)]++;
  }
  a = y[0];
  for (int k = 1; (k < N); k++) {
    transfer(a, y[k]);
  } /*B@*/
  return 0;
}
