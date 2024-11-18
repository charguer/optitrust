#include <optitrust.h>

void f() {
  __pure();

  int x = 0;
  const int st = 0;
  const int N = 10;

  for (int i = 2; i < 10; i++){
    x += i + 2;
  }

  for (int i2 = 0; i2 < 10; i2++){
    x += i2;
  }

  int w = 10 + 2;
  for (int j = st; j < st+N; j++){
    x += j;
  }

  const int shift = 5;
  for (int k = 0; k < N; k++){
    x += k;
  }
/* FIXME:
  for (int l = N; l > 0; l--){
    x += l;
  }
*/
/* FIXME:
  float* input;
  float* output;
  for (int bi = 0; bi < N; bi += 32) {
    for (int i = 0; i < 32; i++) {
      float sum = 0.;
      for (int k2 = 0; k2 < N; k2++) {
        sum += input[k2 + N * (i - -bi)];
      }
      output[N * (i - -bi)] = sum;
    }
  }
*/
}

void ghost_in_range(int* x, int N) {
  for (int m = 2; m < N-2; m++) {
    __ghost([&] {
      __requires("in_range(m, 2..(N-2))");
    }, "");
    x += m;
  }
}

void arrays(int N, int* w, int* r, int* f) {
  __writes("w ~> Matrix1(N)");
  __reads("r ~> Matrix1(N)");
  __modifies("f ~> Matrix1(N)");

  for (int i = 0; i < N; i++) {
    __xwrites("&w[MINDEX1(N, i)] ~> Cell");
    // FIXME: __xreads("&r[MINDEX1(N, i)] ~> Cell");
    __xmodifies("&f[MINDEX1(N, i)] ~> Cell");
    w[MINDEX1(N, i)] = i; /* FIXME: + r[MINDEX1(N, i)]; */
    f[MINDEX1(N, i)] = i + f[MINDEX1(N, i)];
  }
}
