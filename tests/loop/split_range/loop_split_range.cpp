#include <optitrust.h>

void f(){
  __pure();

  int x = 0;
  const int st = 0;
  const int N = 10;

  for (int i = 0; i < 10; i++){
    x += i;
  }

  for (int j = st; j < N; j++){
    x += j;
  }

  const int cut = 5;
  for (int k = 0; k < N; k++){
    x += k;
  }

  for (int l = st; l < N; l++){
    x += l;
  }

}

void array_copy(int* a, int* b, int n) {
  __requires("n >= 0");
  __writes("a ~> Matrix1(n)");
  __reads("b ~> Matrix1(n)");

  for (int i = 0; i < n; i++) {
    __xwrites("&a[MINDEX1(n, i)] ~> Cell");
    __sreads("b ~> Matrix1(n)");

    __GHOST_BEGIN(focus, ro_matrix1_focus, "b, i");
    a[MINDEX1(n, i)] = b[MINDEX1(n, i)];
    __GHOST_END(focus);
  }
}

void non_transparent_ghosts(int* t, int n) {
  __requires("n >= 0");
  __writes("t ~> Matrix1(n)");

  const int cut = 0;
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
