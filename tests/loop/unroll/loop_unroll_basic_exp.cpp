#include <optitrust.h>

void simple(int* t) {
  __writes("for i in 0..3 -> &t[i] ~> Cell");
  int a = 2;
  __ghost(assume, "P := in_range(0, 0..3)");
  __ghost(assume, "P := in_range(1, 0..3)");
  __ghost(assume, "P := in_range(2, 0..3)");
  __ghost([&]() {
    __consumes("for i in 0..3 -> &t[i] ~> UninitCell");
    __produces("&t[0] ~> UninitCell");
    __produces("&t[1] ~> UninitCell");
    __produces("&t[2] ~> UninitCell");
    __admitted();
    __with("justif := unroll");
  });
  t[0] = a;
  t[1] = a;
  t[2] = a;
  __ghost([&]() {
    __consumes("&t[0] ~> Cell");
    __consumes("&t[1] ~> Cell");
    __consumes("&t[2] ~> Cell");
    __produces("for i in 0..3 -> &t[i] ~> Cell");
    __admitted();
    __with("justif := roll");
  });
}

void name_conflict_no_brace() {
  __pure();
  __ghost(assume, "P := in_range(0, 0..2)");
  __ghost(assume, "P := in_range(1, 0..2)");
  int a = 0;
  int a1 = 1;
}

void name_conflict_with_braces() {
  __pure();
  __ghost(assume, "P := in_range(0, 0..2)");
  __ghost(assume, "P := in_range(1, 0..2)");
  { int a = 0; }
  { int a = 1; }
}

void start_add(int a) {
  __pure();
  __ghost(assume, "P := in_range(a, a..(a + 2))");
  __ghost(assume, "P := in_range(a + 1, a..(a + 2))");
  int b = a;
  int b2 = a + 1;
}

void step() {
  int b = 0;
  int b3 = 2;
  int b4 = 4;
  int c = 0;
  int c5 = 3;
}

void iter_contract(int* M) {
  __modifies("M ~> Matrix1(3)");
  __ghost(assume, "P := in_range(0, 0..3)");
  __ghost(assume, "P := in_range(1, 0..3)");
  __ghost(assume, "P := in_range(2, 0..3)");
  __ghost([&]() {
    __consumes("M ~> Matrix1(3)");
    __produces("&M[MINDEX1(3, 0)] ~> Cell");
    __produces("&M[MINDEX1(3, 1)] ~> Cell");
    __produces("&M[MINDEX1(3, 2)] ~> Cell");
    __admitted();
    __with("justif := unroll");
  });
  M[MINDEX1(3, 0)] = 0;
  M[MINDEX1(3, 1)] = 0;
  M[MINDEX1(3, 2)] = 0;
  __ghost([&]() {
    __consumes("&M[MINDEX1(3, 0)] ~> Cell");
    __consumes("&M[MINDEX1(3, 1)] ~> Cell");
    __consumes("&M[MINDEX1(3, 2)] ~> Cell");
    __produces("M ~> Matrix1(3)");
    __admitted();
    __with("justif := roll");
  });
}

void focus_in_range(int* M, int s) {
  __requires("__is_true(s >= 0)");
  __reads("M ~> Matrix1(3)");
  int a = 0;
  __ghost(assume, "P := in_range(0, 0..3)");
  __ghost(assume, "P := in_range(1, 0..3)");
  __ghost(assume, "P := in_range(2, 0..3)");
  __ghost(in_range_bounds, "x := 0, a := 0, b := 3");
  const __ghost_fn focus =
      __ghost_begin(ro_matrix1_focus, "matrix := M, i := 0");
  a += M[MINDEX1(3, 0)];
  __ghost_end(focus);
  __ghost(in_range_bounds, "x := 1, a := 0, b := 3");
  const __ghost_fn focus6 =
      __ghost_begin(ro_matrix1_focus, "matrix := M, i := 1");
  a += M[MINDEX1(3, 1)];
  __ghost_end(focus6);
  __ghost(in_range_bounds, "x := 2, a := 0, b := 3");
  const __ghost_fn focus7 =
      __ghost_begin(ro_matrix1_focus, "matrix := M, i := 2");
  a += M[MINDEX1(3, 2)];
  __ghost_end(focus7);
  int b = 0;
  __ghost(assume, "P := in_range(s, s..(s + 3))");
  __ghost(assume, "P := in_range(s + 1, s..(s + 3))");
  __ghost(assume, "P := in_range(s + 2, s..(s + 3))");
  __ghost(in_range_bounds, "x := s, a := s, b := s + 3");
  __ghost(in_range_bounds, "x := s + 1, a := s, b := s + 3");
  __ghost(in_range_bounds, "x := s + 2, a := s, b := s + 3");
}
