#include "optitrust.h"

///////////////// Simple Cases

// Simple focus: one dimensional focus.
void simple_focus(float *x, int n) {
  __modifies(" &x[MINDEX1(n,2)] ~> Cell");
  __admitted();
}
void simple_focus_caller(float *x, int n) {
  __modifies("for i1 in 0..n -> &x[MINDEX1(n,i1)] ~> Cell");
  simple_focus(x, n);
}

//  Read-only simple focus
void RO_simple_focus(float *y, int n) {
  __reads(" &y[MINDEX1(n,2)] ~> Cell");
  __admitted();
}
void RO_simple_focus_caller(float *x, int m, int n) {
  __reads("for i1 in 0..n -> &x[MINDEX1(n,i1)] ~> Cell");
  RO_simple_focus(x, n);
}

int var_subst_simple_focus(float *x, int n) {
  __modifies(" &x[MINDEX1(n,2)] ~> Cell");
  __admitted();
  return 2;
}
void var_subst_simple_focus_caller(float *x, int n) {
  __modifies("for i1 in 0..n -> &x[MINDEX1(n,i1)] ~> Cell");
  int b = var_subst_simple_focus(x, n);
}
// General_simple_focus: Focus on multi-dimensional array. One focus on the
// third dimension
void general_simple_focus(float *x, int n1b, int n2, int n3, int n4b) {
  __modifies("for i10 in 0..n1b -> for i4 in 0..n4b -> "
             "&x[MINDEX4(n1b,n2,n3,n4b,i10,2,3,i4)] ~> Cell");
  __admitted();
}

void general_simple_focus_caller(float *x, int n1, int n2, int n3, int n4) {
  __modifies("for i1 in 0..n1 -> for i3 in 0..n3 -> for i4 in "
             "0..n4 -> "
             "&x[MINDEX4(n1,n2,n3,n4,i1,2,i3,i4)] ~> Cell");
  general_simple_focus(x, n1, n2, n3, n4);
}

// issues with modifies -- reads -> look at it with arthur
void ro_modifies_focus(float *x, int n1) {
  __reads("&x[MINDEX1(n1,2)]~>Cell");
  __admitted();
}
void ro_modifies_focus_caller(float *x, int n1) {
  __modifies("for i1 in 0..n1 -> &x[MINDEX1(n1,i1)] ~> Cell");
  ro_modifies_focus(x, n1);
}
// Multi-focus: 2 focus steps.
void multi_focus(float *x, int n1, int n2, int n3) {
  __modifies("for i1 in 0..n1 -> &x[MINDEX3(n1,n2,n3,i1,2,3)] ~> Cell");
  __admitted();
}
void multi_focus_caller(float *x, int n1, int n2, int n3) {
  __modifies("for i1 in 0..n1 -> for i2 in 0..n2 -> for i3 in 0..n3 -> "
             "&x[MINDEX3(n1,n2,n3,i1,i2,i3)] ~> Cell");
  multi_focus(x, n1, n2, n3);
}

///////////////// Built-in functions


void get_test(float *x ,int n1)
{
  __reads("for i1 in 0..n1 -> &x[MINDEX1(n1,i1)] ~> Cell");
  float a = x[MINDEX1(n1,2)];
}

void set_test(float *x, int n1) {
  __modifies("for i1 in 0..n1 -> &x[MINDEX1(n1,i1)] ~> Cell");
  x[MINDEX1(n1, 2)] = 3.f;
}

void get_set_test(float*x, float *y, int n){
  __modifies("x ~> Matrix1(n)");
  __reads("y ~> Matrix1(n)");

  x[MINDEX1(n,0)] = y[MINDEX1(n,0)];
}
void test_get_set_same_variable(float *x, float *y, int n) {
  __modifies("x ~> Matrix1(n)");

  x[MINDEX1(n, 0)] = x[MINDEX1(n, 1)] + 1.f;
}

void multi_get(float *x, float *y, int n) {
  __reads("x ~> Matrix1(n)");
  __reads("y ~> Matrix1(n)");
  __pure();
}
void multi_get_caller(float *x, int m, int n) {
  __reads("x ~> Matrix2(m,n)");
  multi_get(&x[MINDEX2(m, n, 0, 0)], &x[MINDEX2(m, n, 1, 0)], n);
}
// void multi_get_values(float x1,float x2){
//   __pure();
//   __admitted();
// }
// void multi_get_values_caller(float*x, int n)
// {
//   __reads("x~>Matrix1(n)");
//   multi_get_values(x[MINDEX1(n,0)], x[MINDEX1(n,1)]);
// }

///////////////// Complex Access Patern
// Indices are compose with sub-expressions

// Complex_access : A star on both focus and candidate sides.
void complex_access(float *x, int n1, int n2) {
  __modifies("for i1 in 0..n1 -> &x[MINDEX2(n1,n2,n1-i1,2)] ~> Cell");
  __admitted();
}
void complex_access_caller(float *x, int n1, int n2) {
  __modifies(
      "for i1 in 0..n1 -> for i2 in 0..n2 -> &x[MINDEX2(n1,n2,n1-i1,i2)] ~> "
      "Cell");
  complex_access(x, n1, n2);
}

// Complex_access_2: Focucs on a "complex" index pattern (i1 must be correctly
// instantiated as c)
void complex_access_2(float *x, int n1, int n2, int c_callee) {
  __modifies("for i2 in 0..n2 -> &x[MINDEX2(n1,n2,(c_callee + c_callee) / "
             "2,i2)] ~> Cell");
  __admitted();
}

void complex_access_caller_2(float *x, int n1, int n2) {
  __modifies("for i1 in 0..n1 -> for i2 in 0..n2 -> &x[MINDEX2(n1,n2,(i1+i1) / "
             "2,i2)] ~> "
             "Cell");
  const int c = 3;
  complex_access_2(x, n1, n2, c);
}

// diag: Permission on a full 2 dimensional array restricted to a diagonal
// i2 must be correctly instantiated as i1
void diag(float *x, int n1) {
  __reads("for i1 in 0..n1 -> &x[MINDEX2(n1,n1,i1,i1)]~> Cell");
  __admitted();
}
void diag_caller(float *x, int n1) {
  __reads(
      "for i1 in 0..n1 -> for i2 in 0..n1 -> &x[MINDEX2(n1,n1,i1,i2)] ~> Cell");
  diag(x, n1);
}

// diag2: permission on a diag focus on a single cell
// instantiation of i1 must be propagated on the rest of indices
void diag2(float *x, int n1) {
  __reads("&x[MINDEX2(n1,n1,2,2)]~> Cell");
  __admitted();
}
void diag2_caller(float *x, int n1) {
  __reads("for i1 in 0..n1 ->  &x[MINDEX2(n1,n1,i1,i1)] ~> Cell");
  diag2(x, n1);
}

////////////// Normalization

// rename
void rename_and_focus(float *y, int n1b, int n2b) {
  __modifies("for j in 0..n2b  -> &y[MINDEX2(n1b,n2b,2,j)] ~> Cell");
  __admitted();
}

void rename_and_focus_caller(float *x, int n1, int n2) {
  __modifies(
      "for i1 in 0..n1 -> for i2 in 0..n2 -> &x[MINDEX2(n1,n2,i1,i2)] ~> Cell");
  rename_and_focus(x, n1, n2);
}

// different_order : Stars mights be in a different order between the caller and
// the callee (even without focus), but it should work

void different_order(float *x, int n1, int n2, int n3) {
  __modifies("for i2 in 0..n2 -> for i3 in 0..n3 -> for i1 in 0..n1 -> "
             "&x[MINDEX3(n1,n2,n3,i1,i2,i3)] ~> Cell");
  __admitted();
}

void different_order_caller(float *x, int n1, int n2, int n3) {
  __modifies("for i1 in 0..n1 -> for i2 in 0..n2 -> for i3 in 0..n3 -> "
             "&x[MINDEX3(n1,n2,n3,i1,i2,i3)] ~> Cell");
  different_order(x, n1, n2, n3);
}

void reorder_and_focus(float *x, int n1, int n2, int n3) {
  __modifies("for i2 in 0..n2 -> for i1 in 0..n1 -> "
             "&x[MINDEX3(n1,n2,n3,i1,i2,2)] ~> Cell");
  __admitted();
}
void reorder_and_focus__caller(float *x, int n1, int n2, int n3) {
  __modifies("for i1 in 0..n1 -> for i2 in 0..n2 -> for i3 in 0..n3 -> "
             "&x[MINDEX3(n1,n2,n3,i1,i2,i3)] ~> Cell");
  reorder_and_focus(x, n1, n2, n3);
}

///////////// Loop
// void loop_basic(float *x, int n1, int n2) {
//   __reads("for i1 in 0..n1 -> for i2 in 0..n2 -> &x[MINDEX2(n1,n2,i1,i2)]
//   ~>Cell"); for (int i1 = 0; i1 < n1; i1++) {
//     for (int i2 = 0; i2 < n2; i2++) {
//       float a = x[MINDEX2(n1, n2, i1, i2)];
//     }
//   }
// }

// void loop_multi_focus(float *x, int n1, int n2, int n3) {
//   __modifies("x ~> 3(n1,n2,n3)");
//   for (int i2 = 0; i2 < n2; i2++) {
//     for (int i3 = 0; i3 < n3; i3++) {
//       x[MINDEX3(n1, n2, n3, 1, i2, i3)] = 2;
//     }
//   }
// }
// /////// EXPECTED WITH LOOP FOCUS
// // void loop_multi_focus(float *x, int n1, int n2, int n3) {
// //   for (int i2 = 0; i2 < n2; i2++) {
// //     for (int i3 = 0; i3 < n3; i3++) {
// // const a = ghostegin(focus1, "for i in 0..n1 -> for i2 in 0..n2 -> for i3
// in
// // 0..n3 -> &x[MINDEX3(n1,n2,n3,i1,i2,i3)] ~>Cell,   i: 1") ; <-- This ghost
// can
// // go outside the loops, const b = ghostegin(focus1, "for i in 0 ..n2 ->
// for
// // i3 in 0..n3 -> &x[MINDEX3(n1,n2,n3,1,i2,i3)] ~>Cell, i := i2"); <-- This
// one
// // can go in between const c = ghostegin(focus1, "for i in 0..n3 ->
// // &x[MINDEX3(n1,n2,n3,1,i2,i3)] ~>Cell, i := i3"); < -- This one must stay
// here
// //       float a = x[MINDEX3(n1, n2, n3, 1, i2, i3)];
// //     }
// //   }
// // }

////////// Rich context

// Multiple resources : Must pick the right resource
void multiple_resources(float *x, int n) {
  __modifies("&x[MINDEX1(n,2)] ~> Cell");
  __admitted();
}

void multiple_resources_caller(float *x, float *y, int n) {
  __modifies("for i1 in 0..n -> &x[MINDEX1(n,i1)] ~> Cell");
  __modifies("for i1 in 0..n -> &y[MINDEX1(n,i1)] ~> Cell");
  multiple_resources(x, n);
  multiple_resources(y, n);
}

// Multiple resource same call : must pick the right resources
void multiple_resources_same_call(float *x, float *y, int n, int m) {
  __modifies("&x[MINDEX1(n,2)] ~> Cell");
  __modifies("&y[MINDEX1(m,2)] ~> Cell");
  __admitted();
}
void multiple_resources_same_call_caller(float *x, float *y, int n, int m) {
  __modifies("for i1 in 0..n -> &x[MINDEX1(n,i1)] ~> Cell");
  __modifies("for i1 in 0..m -> &y[MINDEX1(m,i1)] ~> Cell");
  multiple_resources_same_call(x, y, n, m);
}
// void indirection(float *x, int *y, int n1, int n2) {
//   __reads("&x[MINDEX1(n1,(&y[MINDEX1(n2,1)]))] ~> Cell");
//   __admitted();
// }
// void indirection_caller(float *x, int *y, int n1, int n2) {
//   __reads("for i1 in 0..n1 -> &x[MINDEX1(n1,y[MINDEX1(n2,i1)])] ~> Cell");
//   indirection(x, y, n1, n2);
// }

// void not_good_range(float *x, int n1, int n2){
// __modifies(
//       "for i1 in 0..n2  -> &x[MINDEX2(n1,n2,i1,2)] ~> "
//       "Cell");
//       __admitted();
// }
// void not_good_range_caller(float *x, int n1, int n2){
//   __modifies(
//       "for i1 in 0..n1 -> for i2 in 0..n2 -> &x[MINDEX2(n1,n2,n1,i2)] ~> "
//       "Cell");
//   not_good_range(x,n1,n2);
// }
