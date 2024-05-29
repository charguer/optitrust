#include <optitrust.h>

void one_fork() {
  __pure();
  int x = 0;
  const __ghost_fn fork_out =
      __ghost_begin(ro_fork_group, "H := &x ~> Cell, r := 0..5");
  for (int i = 0; i < 5; i++) {
    __strict();
    __xreads("&x ~> Cell");
    __ghost(ro_split2, "f := #_1, H := &x ~> Cell");
    __ghost(ro_fork_group, "f := #_1 / 2, H := &x ~> Cell, r := 0..5");
    __ghost(ro_allow_join2, "f := #_1, H := &x ~> Cell");
    for (int j = 0; j < 5; j++) {
      __strict();
      __xreads("&x ~> Cell");
      x + 1;
    }
    __ghost(ro_join_group, "H := &x ~> Cell, r := 0..5");
  }
  __ghost_end(fork_out);
}

void two_forks() {
  __pure();
  int x = 0;
  const __ghost_fn fork_out =
      __ghost_begin(ro_fork_group, "H := &x ~> Cell, r := 0..5");
  for (int i = 0; i < 5; i++) {
    __strict();
    __xreads("&x ~> Cell");
    __ghost(ro_split3, "f := #_1, H := &x ~> Cell");
    __ghost(ro_fork_group, "f := #_1 / 3, H := &x ~> Cell, r := 0..5");
    __ghost(ro_fork_group, "f := #_1 / 3, H := &x ~> Cell, r := 0..5");
    __ghost(ro_allow_join3, "f := #_1, H := &x ~> Cell");
    for (int j = 0; j < 5; j++) {
      __strict();
      __xreads("&x ~> Cell");
      x + 1;
    }
    __ghost(ro_join_group, "H := &x ~> Cell, r := 0..5");
    __ghost(ro_join_group, "H := &x ~> Cell, r := 0..5");
  }
  __ghost_end(fork_out);
}

void two_forks_spe_twice() {
  __pure();
  int x = 0;
  const __ghost_fn fork_out =
      __ghost_begin(ro_fork_group, "H := &x ~> Cell, r := 0..5");
  for (int i = 0; i < 5; i++) {
    __strict();
    __xreads("&x ~> Cell");
    __ghost(ro_split2, "f := #_1, H := &x ~> Cell");
    __ghost(ro_split2, "f := #_1 / 2, H := &x ~> Cell");
    __ghost(ro_fork_group, "f := #_1 / 2, H := &x ~> Cell, r := 0..5");
    __ghost(ro_fork_group, "f := #_1 / 2 / 2, H := &x ~> Cell, r := 0..5");
    __ghost(ro_allow_join2, "f := #_1 / 2, H := &x ~> Cell");
    for (int j = 0; j < 5; j++) {
      __strict();
      __xreads("&x ~> Cell");
      x + 1;
    }
    __ghost(ro_join_group, "H := &x ~> Cell, r := 0..5");
    __ghost(ro_allow_join2, "f := #_1, H := &x ~> Cell");
    __ghost(ro_join_group, "H := &x ~> Cell, r := 0..5");
  }
  __ghost_end(fork_out);
}

void fork_then_write(float* M) {
  __modifies("M ~> Matrix1(5)");
  for (int i = 0; i < 5; i++) {
    __strict();
    __xmodifies("&M[MINDEX1(5, i)] ~> Cell");
    __ghost(ro_split2, "f := 1, H := &M[MINDEX1(5, i)] ~> Cell");
    __ghost(ro_fork_group,
            "f := 1 / 2, H := &M[MINDEX1(5, i)] ~> Cell, r := 0..5");
    __ghost(ro_allow_join2, "f := 1, H := &M[MINDEX1(5, i)] ~> Cell");
    for (int j = 0; j < 5; j++) {
      __strict();
      __xreads("&M[MINDEX1(5, i)] ~> Cell");
      M[MINDEX1(5, i)] + 1;
    }
    __ghost(ro_join_group, "H := &M[MINDEX1(5, i)] ~> Cell, r := 0..5");
    M[MINDEX1(5, i)] = i;
  }
}

void read_then_fork(float* M) {
  __reads("M ~> Matrix1(5)");
  for (int i = 0; i < 5; i++) {
    __strict();
    __xreads("&M[MINDEX1(5, i)] ~> Cell");
    __ghost(ro_split2, "f := #_2, H := &M[MINDEX1(5, i)] ~> Cell");
    M[MINDEX1(5, i)] + i;
    __ghost(ro_fork_group,
            "f := #_2 / 2, H := &M[MINDEX1(5, i)] ~> Cell, r := 0..5");
    M[MINDEX1(5, i)] + i;
    __ghost(ro_allow_join2, "f := #_2, H := &M[MINDEX1(5, i)] ~> Cell");
    for (int j = 0; j < 5; j++) {
      __strict();
      __xreads("&M[MINDEX1(5, i)] ~> Cell");
      M[MINDEX1(5, i)] + 1;
    }
    __ghost(ro_join_group, "H := &M[MINDEX1(5, i)] ~> Cell, r := 0..5");
  }
}

void write_then_fork(float* M) {
  __writes("M ~> Matrix1(5)");
  for (int i = 0; i < 5; i++) {
    __strict();
    __xwrites("&M[MINDEX1(5, i)] ~> Cell");
    M[MINDEX1(5, i)] = i;
    __ghost(ro_split2, "f := 1, H := &M[MINDEX1(5, i)] ~> Cell");
    __ghost(ro_fork_group,
            "f := 1 / 2, H := &M[MINDEX1(5, i)] ~> Cell, r := 0..5");
    __ghost(ro_allow_join2, "f := 1, H := &M[MINDEX1(5, i)] ~> Cell");
    for (int j = 0; j < 5; j++) {
      __strict();
      __xreads("&M[MINDEX1(5, i)] ~> Cell");
      M[MINDEX1(5, i)] + 1;
    }
    __ghost(ro_join_group, "H := &M[MINDEX1(5, i)] ~> Cell, r := 0..5");
  }
}
