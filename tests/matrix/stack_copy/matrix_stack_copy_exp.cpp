#include <optitrust.h>

void copy1from2(int* s) {
  __modifies("s ~> Matrix2(32, 32)");
  for (int i = 0; i < 32; i++) {
    __strict();
    __xmodifies("for j in 0..32 -> &s[MINDEX2(32, 32, i, j)] ~> Cell");
    int x[MSIZE1(32)];
    const __ghost_fn __ghost_pair_1 = __ghost_begin(
        ro_mindex2_unfold,
        "H := fun (access: int * int -> int*) -> for j in 0..32 -> access(i, "
        "j) ~> Cell, matrix := s, n1 := 32, n2 := 32");
    MATRIX1_COPY_int(x, &s[i * 32], 32);
    __ghost_end(__ghost_pair_1);
    for (int j = 0; j < 32; j++) {
      __strict();
      __xmodifies("&x[MINDEX1(32, j)] ~> Cell");
      for (int k = 0; k < 4; k++) {
        __strict();
        __smodifies("&x[MINDEX1(32, j)] ~> Cell");
        x[MINDEX1(32, j)] += k;
      }
    }
    __ghost(mindex2_unfold,
            "H := fun (access: int * int -> int*) -> for j in 0..32 -> "
            "access(i, j) ~> UninitCell, matrix := s, n1 := 32, n2 := 32");
    MATRIX1_COPY_int(&s[i * 32], x, 32);
    __ghost(mindex2_fold,
            "H := fun (access: int * int -> int*) -> for j in 0..32 -> "
            "access(i, j) ~> Cell, matrix := s, n1 := 32, n2 := 32");
  }
}

void copy1from3(int* s) {
  __modifies("s ~> Matrix3(32, 32, 32)");
  for (int i = 0; i < 32; i++) {
    __strict();
    __xmodifies(
        "for j in 0..32 -> for k in 0..32 -> &s[MINDEX3(32, 32, 32, i, j, k)] "
        "~> Cell");
    for (int j = 0; j < 32; j++) {
      __strict();
      __xmodifies("for k in 0..32 -> &s[MINDEX3(32, 32, 32, i, j, k)] ~> Cell");
      int x[MSIZE1(32)];
      const __ghost_fn __ghost_pair_2 = __ghost_begin(
          ro_mindex3_unfold,
          "H := fun (access: int * int * int -> int*) -> for k in 0..32 -> "
          "access(i, j, k) ~> Cell, matrix := s, n1 := 32, n2 := 32, n3 := 32");
      const __ghost_fn __ghost_pair_3 = __ghost_begin(
          ro_mindex2_unfold,
          "H := fun (access: int * int -> int*) -> for k in 0..32 -> access(j, "
          "k) ~> Cell, matrix := &s[i * 32 * 32], n1 := 32, n2 := 32");
      MATRIX1_COPY_int(x, &(&s[i * 32 * 32])[j * 32], 32);
      __ghost_end(__ghost_pair_3);
      __ghost_end(__ghost_pair_2);
      for (int k = 0; k < 32; k++) {
        __strict();
        __xmodifies("&x[MINDEX1(32, k)] ~> Cell");
        x[MINDEX1(32, k)] += k;
      }
      __ghost(mindex3_unfold,
              "H := fun (access: int * int * int -> int*) -> for k in 0..32 -> "
              "access(i, j, k) ~> UninitCell, matrix := s, n1 := 32, n2 := 32, "
              "n3 := 32");
      __ghost(
          mindex2_unfold,
          "H := fun (access: int * int -> int*) -> for k in 0..32 -> access(j, "
          "k) ~> UninitCell, matrix := &s[i * 32 * 32], n1 := 32, n2 := 32");
      MATRIX1_COPY_int(&(&s[i * 32 * 32])[j * 32], x, 32);
      __ghost(
          mindex2_fold,
          "H := fun (access: int * int -> int*) -> for k in 0..32 -> access(j, "
          "k) ~> Cell, matrix := &s[i * 32 * 32], n1 := 32, n2 := 32");
      __ghost(
          mindex3_fold,
          "H := fun (access: int * int * int -> int*) -> for k in 0..32 -> "
          "access(i, j, k) ~> Cell, matrix := s, n1 := 32, n2 := 32, n3 := 32");
    }
  }
}

void copy2from3(int* s) {
  __modifies("s ~> Matrix3(32, 32, 32)");
  for (int i = 0; i < 32; i++) {
    __strict();
    __xmodifies(
        "for j in 0..32 -> for k in 0..32 -> &s[MINDEX3(32, 32, 32, i, j, k)] "
        "~> Cell");
    int x[MSIZE2(32, 32)];
    const __ghost_fn __ghost_pair_4 =
        __ghost_begin(ro_mindex3_unfold,
                      "H := fun (access: int * int * int -> int*) -> for j in "
                      "0..32 -> for k in 0..32 -> access(i, j, k) ~> Cell, "
                      "matrix := s, n1 := 32, n2 := 32, n3 := 32");
    MATRIX2_COPY_int(x, &s[i * 32 * 32], 32, 32);
    __ghost_end(__ghost_pair_4);
    for (int j = 0; j < 32; j++) {
      __strict();
      __xmodifies("for k in 0..32 -> &x[MINDEX2(32, 32, j, k)] ~> Cell");
      for (int k = 0; k < 32; k++) {
        __strict();
        __xmodifies("&x[MINDEX2(32, 32, j, k)] ~> Cell");
        x[MINDEX2(32, 32, j, k)] += k;
      }
    }
    __ghost(mindex3_unfold,
            "H := fun (access: int * int * int -> int*) -> for j in 0..32 -> "
            "for k in 0..32 -> access(i, j, k) ~> UninitCell, matrix := s, n1 "
            ":= 32, n2 := 32, n3 := 32");
    MATRIX2_COPY_int(&s[i * 32 * 32], x, 32, 32);
    __ghost(mindex3_fold,
            "H := fun (access: int * int * int -> int*) -> for j in 0..32 -> "
            "for k in 0..32 -> access(i, j, k) ~> Cell, matrix := s, n1 := 32, "
            "n2 := 32, n3 := 32");
  }
}
