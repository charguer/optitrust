#include <optitrust_models.h>

void one_fork() {
  __pure();
  int x = 0;
  const __ghost_fn fork_out =
      __ghost_begin(ro_fork_group, "H := &x ~~> 0, r := 0..5");
  for (int i = 0; i < 5; i++) {
    __strict();
    __xreads("&x ~~> 0");
    __ghost(ro_split2, "f := #_1, H := &x ~~> 0");
    __ghost(ro_fork_group, "f := #_1 / 2, H := &x ~~> 0, r := 0..5");
    __ghost(ro_allow_join2, "f := #_1, H := &x ~~> 0");
    for (int j = 0; j < 5; j++) {
      __strict();
      __xreads("&x ~~> 0");
      x + 1;
    }
    __ghost(ro_join_group, "H := &x ~~> 0, r := 0..5");
  }
  __ghost_end(fork_out);
}

void two_forks() {
  __pure();
  int x = 0;
  const __ghost_fn fork_out =
      __ghost_begin(ro_fork_group, "H := &x ~~> 0, r := 0..5");
  for (int i = 0; i < 5; i++) {
    __strict();
    __xreads("&x ~~> 0");
    __ghost(ro_split3, "f := #_1, H := &x ~~> 0");
    __ghost(ro_fork_group, "f := #_1 / 3, H := &x ~~> 0, r := 0..5");
    __ghost(ro_fork_group, "f := #_1 / 3, H := &x ~~> 0, r := 0..5");
    __ghost(ro_allow_join3, "f := #_1, H := &x ~~> 0");
    for (int j = 0; j < 5; j++) {
      __strict();
      __xreads("&x ~~> 0");
      x + 1;
    }
    __ghost(ro_join_group, "H := &x ~~> 0, r := 0..5");
    __ghost(ro_join_group, "H := &x ~~> 0, r := 0..5");
  }
  __ghost_end(fork_out);
}

void two_forks_spe_twice() {
  __pure();
  int x = 0;
  const __ghost_fn fork_out =
      __ghost_begin(ro_fork_group, "H := &x ~~> 0, r := 0..5");
  for (int i = 0; i < 5; i++) {
    __strict();
    __xreads("&x ~~> 0");
    __ghost(ro_split2, "f := #_1, H := &x ~~> 0");
    __ghost(ro_split2, "f := #_1 / 2, H := &x ~~> 0");
    __ghost(ro_fork_group, "f := #_1 / 2, H := &x ~~> 0, r := 0..5");
    __ghost(ro_fork_group, "f := #_1 / 2 / 2, H := &x ~~> 0, r := 0..5");
    __ghost(ro_allow_join2, "f := #_1 / 2, H := &x ~~> 0");
    for (int j = 0; j < 5; j++) {
      __strict();
      __xreads("&x ~~> 0");
      x + 1;
    }
    __ghost(ro_join_group, "H := &x ~~> 0, r := 0..5");
    __ghost(ro_allow_join2, "f := #_1, H := &x ~~> 0");
    __ghost(ro_join_group, "H := &x ~~> 0, r := 0..5");
  }
  __ghost_end(fork_out);
}
