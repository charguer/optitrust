#include <optitrust.h>

void one_fork() {
  __pure();

  int x = 0;
  __GHOST_BEGIN(fork_out, ro_fork_group, "H := &x ~> Cell, r := 0..5");
  for (int i = 0; i < 5; i++) {
    __strict();
    __xreads("&x ~> Cell");
    __ghost(ro_fork_group, "H := &x ~> Cell, r := 0..5");
    for (int j = 0; j < 5; j++) {
      __strict();
      __xreads("&x ~> Cell");
      x + 1;
    }
    __ghost(ro_join_group, "H := &x ~> Cell, r := 0..5");
  }
  __GHOST_END(fork_out);
}

void two_forks() {
  __pure();

  int x = 0;
  __GHOST_BEGIN(fork_out, ro_fork_group, "H := &x ~> Cell, r := 0..5");
  for (int i = 0; i < 5; i++) {
    __strict();
    __xreads("&x ~> Cell");
    __ghost(ro_fork_group, "H := &x ~> Cell, r := 0..5");
    __ghost(ro_fork_group, "H := &x ~> Cell, r := 0..5");
    for (int j = 0; j < 5; j++) {
      __strict();
      __xreads("&x ~> Cell");
      x + 1;
    }
    __ghost(ro_join_group, "H := &x ~> Cell, r := 0..5");
    __ghost(ro_join_group, "H := &x ~> Cell, r := 0..5");
  }
  __GHOST_END(fork_out);
}

void two_forks_spe_twice() {
  __pure();

  int x = 0;
  __GHOST_BEGIN(fork_out, ro_fork_group, "H := &x ~> Cell, r := 0..5");
  for (int i = 0; i < 5; i++) {
    __strict();
    __xreads("&x ~> Cell");
    __ghost(ro_fork_group, "H := &x ~> Cell, r := 0..5");
    __ghost(ro_fork_group, "H := &x ~> Cell, r := 0..5");
    for (int j = 0; j < 5; j++) {
      __strict();
      __xreads("&x ~> Cell");
      x + 1;
    }
    __ghost(ro_join_group, "H := &x ~> Cell, r := 0..5");
    __ghost(ro_join_group, "H := &x ~> Cell, r := 0..5");
  }
  __GHOST_END(fork_out);
}

void fork_then_write(float* M) {
  __modifies("M ~> Matrix1(5)");

  for (int i = 0; i < 5; i++) {
    __strict();
    __xmodifies("&M[MINDEX1(5,i)] ~> Cell");
    __ghost(ro_fork_group, "H := &M[MINDEX1(5,i)] ~> Cell, r := 0..5");
    for (int j = 0; j < 5; j++) {
      __strict();
      __xreads("&M[MINDEX1(5,i)] ~> Cell");
      M[MINDEX1(5,i)] + 1;
    }
    __ghost(ro_join_group, "H := &M[MINDEX1(5,i)] ~> Cell, r := 0..5");
    M[MINDEX1(5,i)] = i;
  }
}

void read_then_fork(float* M) {
  __reads("M ~> Matrix1(5)");

  for (int i = 0; i < 5; i++) {
    __strict();
    __xreads("&M[MINDEX1(5,i)] ~> Cell");
    M[MINDEX1(5,i)] + i;
    __ghost(ro_fork_group, "H := &M[MINDEX1(5,i)] ~> Cell, r := 0..5");
    M[MINDEX1(5,i)] + i;
    for (int j = 0; j < 5; j++) {
      __strict();
      __xreads("&M[MINDEX1(5,i)] ~> Cell");
      M[MINDEX1(5,i)] + 1;
    }
    __ghost(ro_join_group, "H := &M[MINDEX1(5,i)] ~> Cell, r := 0..5");
  }
}

void write_then_fork(float* M) {
  __writes("M ~> Matrix1(5)");

  for (int i = 0; i < 5; i++) {
    __strict();
    __xwrites("&M[MINDEX1(5,i)] ~> Cell");
    M[MINDEX1(5,i)] = i;
    __ghost(ro_fork_group, "H := &M[MINDEX1(5,i)] ~> Cell, r := 0..5");
    for (int j = 0; j < 5; j++) {
      __strict();
      __xreads("&M[MINDEX1(5,i)] ~> Cell");
      M[MINDEX1(5,i)] + 1;
    }
    __ghost(ro_join_group, "H := &M[MINDEX1(5,i)] ~> Cell, r := 0..5");
  }
}
