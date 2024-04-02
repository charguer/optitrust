#include <optitrust.h>

void parallel(int* t, int* u, int n) {
  __modifies(
    "for i in 1..n -> &t[i] ~> Cell,"
    "for i in 1..n -> &u[i] ~> Cell");

  for (int i = 1; i < n; i++) {
    __strict();
    __modifies(
      "&t[i] ~> Cell,"
      "&u[i] ~> Cell");
    int a = i;
    t[i] += a;
    int b = i;
    u[i] += b + t[i];
    int c = i;
    u[i] += c;
  }

  for (int i = 0; (i < 5); i++) {
    __strict();
    int y = i;
    int z = i;
  }

  for (int i = 0; i < 3; i++) {
    __strict();
    int* const m1 = (int* const) MALLOC1(5, sizeof(int));
    MFREE1(5, m1);
    int* const m2 = (int* const) MALLOC1(5, sizeof(int));
    MFREE1(5, m2);
  }
}

void uninit(int* t, int* u, int n) {
  __consumes("_Uninit(for i in 1..n -> &t[i] ~> Cell)");
  __produces("for i in 1..n -> &t[i] ~> Cell");

  int x = 0;
  for (int i = 1; i < n; i++) {
    __strict();
    __sequentially_modifies("&x ~> Cell");
    __consumes("_Uninit(&t[i] ~> Cell)");
    __produces("&t[i] ~> Cell");

    t[i] = i;
    x += t[i];
  }
}

void commute() {
  __pure();

  int x;
  int y;
  for (int i = 0; i < 5; i++) {
    __strict();
    __sequentially_modifies(
      "&x ~> Cell,"
      "&y ~> Cell");
    x++;
    y++;
  }

  int z = 2;
  for (int j = 0; j < 5; j++) {
    __strict();
    __parallel_reads("&z ~> Cell");
    __sequentially_modifies(
      "&x ~> Cell,"
      "&y ~> Cell");
    x += z;
    y += z;
  }

  for (int k1 = 0; k1 < 5; k1++) {
    __strict();
    __sequentially_modifies(
      "&x ~> Cell,"
      "&y ~> Cell,"
      "&z ~> Cell");

    x += 1;
    for (int k2 = 0; k2 < 5; k2++) {
      __strict();
      __sequentially_modifies("&y ~> Cell");
      y += 1;
    }
    z += 1;
  }
}

void wrong_rw_rw() {
  __pure();

  int x = 0;
  for (int i = 0; i < 4; i++) {
    __strict();
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
    __strict();
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
    __strict();
    __sequentially_modifies("&x ~> Cell");
    __sequentially_modifies("&y ~> Cell");
    y += x;
    x++;
  }
}

void empty() {
  __pure();

  for (int i = 0; i < 1024; ++i) {
    __strict();
    // empty
  }
}

int testAllInstr(int* t, int* u, int n) {
  __pure();
  for (int i = 0; (i < 5); i++) {
    __strict();
    int a1 = i;
    int a2 = i;
    int a3 = i;
    int a4 = i;
    int a5 = i;
  }
}

int testAllInstr2(int* t, int* u, int n) {
  __pure();
  for (int i = 0; (i < 5); i++) {
    __strict();
    int a1 = i;
    int a2 = i;
    int a3 = i;
    int a4 = i;
    int a5 = i;
  }
}

int testAllInstrContracts(int* t, int* u, int n) {
    __modifies(
    "for i in 1..n -> &t[i] ~> Cell,"
    "for i in 1..n -> &u[i] ~> Cell");
  for (int i = 1; i < n; i++) {
    __strict();
    __modifies(
      "&t[i] ~> Cell,"
      "&u[i] ~> Cell");
    int a = i;
    t[i] += a;
    int b = i;
    u[i] += b;
    int c = i;
    u[i] += c; // TODO replace c with b and what is the error message
  }

}

void ghosts() {
  __pure();

  int x = 0;
  const __ghost_fn __ghost_pair_1 =
      __ghost_begin(ro_fork_group, "H := &x ~> Cell, r := 0..5");
  for (int i = 0; i < 5; i++) {
    __strict();
    __reads("&x ~> Cell");
    __ghost(ro_fork_group, "H := &x ~> Cell, r := 0..5");
    for (int k = 0; k < 5; k++) {
      __strict();
      __parallel_reads("for j in 0..5 -> &x ~> Cell");
      for (int j = 0; j < 5; j++) {
        __strict();
        __reads("&x ~> Cell");
        x + 1;
      }
    }
    __ghost(ro_join_group, "H := &x ~> Cell, r := 0..5");
  }
  __ghost_end(__ghost_pair_1);
}

void double_ghosts() {
  __pure();

  int x = 0;
  const __ghost_fn __ghost_pair_1 =
      __ghost_begin(ro_fork_group, "H := &x ~> Cell, r := 0..5");
  for (int i = 0; i < 5; i++) {
    __strict();
    __reads("&x ~> Cell");
    __ghost(ro_fork_group, "H := &x ~> Cell, r := 0..5");
    __ghost(ro_fork_group, "H := &x ~> Cell, r := 0..5");
    for (int k = 0; k < 5; k++) {
      __strict();
      __parallel_reads("for j in 0..5 -> &x ~> Cell");
      for (int j = 0; j < 5; j++) {
        __strict();
        __reads("&x ~> Cell");
        x + 1;
      }
    }
    __ghost(ro_join_group, "H := &x ~> Cell, r := 0..5");
    __ghost(ro_join_group, "H := &x ~> Cell, r := 0..5");
  }
  __ghost_end(__ghost_pair_1);
}
