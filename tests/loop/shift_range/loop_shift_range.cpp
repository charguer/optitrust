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

void non_transparent_ghosts(int* t, int n) {
  __writes("t ~> Matrix1(n)");

  __ghost(group_intro_zero, "items := fun i -> &t[MINDEX1(n, i)] ~> Cell");
  for (int i = 0; i < n; i++) {
    __smodifies("for i in 0..i -> &t[MINDEX1(n, i)] ~> Cell");
    __smodifies("for i in i..n -> &t[MINDEX1(n, i)] ~> UninitCell");

    __ghost(assume, "is_subrange(i..(i + 1), i..n)");
    __ghost(group_split, "split := i + 1, items := fun i -> &t[MINDEX1(n, i)] ~> UninitCell");
    for (int j = i; j < i + 1; j++) {
      __xwrites("&t[MINDEX1(n, j)] ~> Cell");
      t[MINDEX1(n, j)] = j;
    }
    __ghost(group_join, "split := i, items := fun i -> &t[MINDEX1(n, i)] ~> Cell");
  }
  __ghost(group_shift, "start := n, stop := n, shift := -n, new_start := 0, new_stop := 0, items := fun i -> &t[MINDEX1(n, i)] ~> UninitCell");
  __ghost(group_elim_zero, "items := fun i -> &t[MINDEX1(n, i - (-n))] ~> UninitCell");
}
