#include <optitrust.h>

void parallel(int* t, int* u, int n) {
  __modifies("Group(range(1, n, 1), fun i -> &t[i] ~> Cell)");
  __modifies("Group(range(1, n, 1), fun i -> &u[i] ~> Cell)");
  for (int i = 1; i < n; i++) {
    __modifies("&t[i] ~> Cell");
    __modifies("&u[i] ~> Cell");
    int a = i;
    t[i] += a;
  }
  for (int i = 1; i < n; i++) {
    __modifies("&t[i] ~> Cell");
    __modifies("&u[i] ~> Cell");
    int b = i;
    u[i] += b + t[i];
    int c = i;
    u[i] += c;
  }
  for (int i = 0; i < 5; i++) {
    __pure();
    int y = i;
  }
  for (int i = 0; i < 5; i++) {
    __pure();
    int z = i;
  }
  for (int i = 0; i < 3; i++) {
    __pure();
    int* const m1 = (int* const)MALLOC1(5, sizeof(int));
    MFREE1(5, m1);
  }
  for (int i = 0; i < 3; i++) {
    __pure();
    int* const m2 = (int* const)MALLOC1(5, sizeof(int));
    MFREE1(5, m2);
  }
}

void uninit(int* t, int* u, int n) {
  __consumes("_Uninit(Group(range(1, n, 1), fun i -> &t[i] ~> Cell))");
  __produces("Group(range(1, n, 1), fun i -> &t[i] ~> Cell)");
  int x = 0;
  for (int i = 1; i < n; i++) {
    __consumes("_Uninit(&t[i] ~> Cell)");
    __produces("&t[i] ~> Cell");
    t[i] = i;
  }
  for (int i = 1; i < n; i++) {
    __sequentially_modifies("&x ~> Cell");
    __modifies("&t[i] ~> Cell");
    x += t[i];
  }
}

void commute() {
  __pure();
  int x;
  int y;
  for (int i = 0; i < 5; i++) {
    __sequentially_modifies("&x ~> Cell");
    x++;
  }
  for (int i = 0; i < 5; i++) {
    __sequentially_modifies("&y ~> Cell");
    y++;
  }
  int z = 2;
  for (int j = 0; j < 5; j++) {
    __sequentially_modifies("&x ~> Cell");
    __sequentially_reads("&z ~> Cell");
    x += z;
  }
  for (int j = 0; j < 5; j++) {
    __sequentially_modifies("&y ~> Cell");
    __sequentially_reads("&z ~> Cell");
    y += z;
  }
  for (int k1 = 0; k1 < 5; k1++) {
    __sequentially_modifies("&x ~> Cell");
    x += 1;
  }
  for (int k1 = 0; k1 < 5; k1++) {
    __sequentially_modifies("&y ~> Cell");
    for (int k2 = 0; k2 < 5; k2++) {
      __sequentially_modifies("&y ~> Cell");
      y += 1;
    }
  }
  for (int k1 = 0; k1 < 5; k1++) {
    __sequentially_modifies("&z ~> Cell");
    z += 1;
  }
}

void wrong_rw_rw() {
  __pure();
  int x = 0;
  for (int i = 0; i < 4; i++) {
    __sequentially_modifies("&x ~> Cell");
    x++;
    x++;
  }
}

void wrong_rw_ro() {
  __pure();
  int x = 0;
  int y = 0;
  for (int i = 0; i < 4; i++) {
    __sequentially_modifies("&x ~> Cell");
    __sequentially_modifies("&y ~> Cell");
    x++;
    y += x;
  }
}

void wrong_ro_rw() {
  __pure();
  int x = 0;
  int y = 0;
  for (int i = 0; i < 4; i++) {
    __sequentially_modifies("&x ~> Cell");
    __sequentially_modifies("&y ~> Cell");
    y += x;
    x++;
  }
}

int testAllInstr(int* t, int* u, int n) {
  __pure();
  for (int i = 0; i < 5; i++) {
    __pure();
    int a1 = i;
  }
  for (int i = 0; i < 5; i++) {
    __pure();
    int a2 = i;
  }
  for (int i = 0; i < 5; i++) {
    __pure();
    int a3 = i;
  }
  for (int i = 0; i < 5; i++) {
    __pure();
    int a4 = i;
  }
  for (int i = 0; i < 5; i++) {
    __pure();
    int a5 = i;
  }
}

int testAllInstr2(int* t, int* u, int n) {
  __pure();
  for (int i = 0; i < 5; i++) {
    __pure();
    int a1 = i;
    int a2 = i;
  }
  for (int i = 0; i < 5; i++) {
    __pure();
    int a3 = i;
  }
  for (int i = 0; i < 5; i++) {
    __pure();
    int a4 = i;
    int a5 = i;
  }
}

int testAllInstrContracts(int* t, int* u, int n) {
  __modifies("Group(range(1, n, 1), fun i -> &t[i] ~> Cell)");
  __modifies("Group(range(1, n, 1), fun i -> &u[i] ~> Cell)");
  for (int i = 1; i < n; i++) {
    __modifies("&t[i] ~> Cell");
    __modifies("&u[i] ~> Cell");
    int a = i;
    t[i] += a;
  }
  for (int i = 1; i < n; i++) {
    __modifies("&t[i] ~> Cell");
    __modifies("&u[i] ~> Cell");
    int b = i;
    u[i] += b;
  }
  for (int i = 1; i < n; i++) {
    __modifies("&u[i] ~> Cell");
    __modifies("&t[i] ~> Cell");
    int c = i;
    u[i] += c;
  }
}

void ghosts() {
  __pure();
  int x = 0;
  const __ghost_fn __ghost_pair_1 =
      __ghost_begin(ro_fork_group, "H := &x ~> Cell, r := range(0, 5, 1)");
  for (int i = 0; i < 5; i++) {
    __loop_ghosts("#134: _Fraction");
    __consumes("_RO(#134, &x ~> Cell)");
    __produces(
        "_RO(#134 / range_count(range(0, 5, 1)), Group(range(0, 5, 1), fun _ "
        "-> &x ~> Cell))");
    __ghost(ro_fork_group, "H := &x ~> Cell, r := range(0, 5, 1)");
  }
  for (int i = 0; i < 5; i++) {
    __loop_ghosts("#134: _Fraction");
    __consumes(
        "_RO(#134 / range_count(range(0, 5, 1)), Group(range(0, 5, 1), fun _ "
        "-> &x ~> Cell))");
    __produces("_RO(#134, &x ~> Cell)");
    for (int k = 0; k < 5; k++) {
      __sequentially_reads("Group(range(0, 5, 1), fun j -> &x ~> Cell)");
      for (int j = 0; j < 5; j++) {
        __reads("&x ~> Cell");
        x + 1;
      }
    }
    __ghost(ro_join_group, "H := &x ~> Cell, r := range(0, 5, 1)");
  }
  __ghost_end(__ghost_pair_1);
}

  void double_ghosts() {
  __pure();
  int x = 0;
    const __ghost_fn __ghost_pair_1 =
      __ghost_begin(ro_fork_group, "H := &x ~> Cell, r := range(0, 5, 1)");
  for (int i = 0; i < 5; i++) {
    __reads("&x ~> Cell");
    __ghost(ro_fork_group, "H := &x ~> Cell, r := range(0, 5, 1)");
    __ghost(ro_fork_group, "H := &x ~> Cell, r := range(0, 5, 1)");
    for (int k = 0; k < 5; k++) {
      __sequentially_reads("Group(range(0, 5, 1), fun j -> &x ~> Cell)");
      for (int j = 0; j < 5; j++) {
        __reads("&x ~> Cell");
        x + 1;
      }
    }
    __ghost(ro_join_group, "H := &x ~> Cell, r := range(0, 5, 1)");
    __ghost(ro_join_group, "H := &x ~> Cell, r := range(0, 5, 1)");
  }
  __ghost_end(__ghost_pair_1);
}
