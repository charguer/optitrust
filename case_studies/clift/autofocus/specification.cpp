#include "optitrust.h"

// Simple focus: We want to type the simple_focus call that requires permission
// on one row of the matrix
void simple_focus(float *y, int n) {
  __modifies(" &y[MINDEX1(n,2)] ~> Cell");
  float a = y[MINDEX1(n, 2)];
}

void simple_focus_caller(float *x, int m, int n) {
  __modifies("for i1 in 0..n -> &x[MINDEX1(n,i1)] ~> Cell");
  simple_focus(x, n);
}
void RO_simple_focus(float *y, int n) {
  __reads(" &y[MINDEX1(n,2)] ~> Cell");
  __admitted();
}

void RO_simple_focus_caller(float *x, int m, int n) {
  __reads("for i1 in 0..n -> &x[MINDEX1(n,i1)] ~> Cell");
  RO_simple_focus(x, n);
}
// General_simple_focus: MINDEX4. The focus can be placed on an inner
// dimension,
// which means we should preserve information about both what comes before
// and
// what follows, any dimension can be already focuses

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

// // Complex access: we have permission on x[MINDEX2(n1,n2, f(i1), i2)]. Should
// // work when the f(i1) is also asked by the callee , abort when different
// access
// // or when a focus is specified on this ?

void complex_access_ok(float *x, int n1_b, int n2_b) {
  __modifies(
      "for i1_b in 0..n1_b -> &x[MINDEX2(n1_b,n2_b,n1_b-i1_b,2)] ~> Cell");
  __admitted();
}
void complex_access_ok_caller(float *x, int n1, int n2) {
  __modifies(
      "for i1 in 0..n1 -> for i2 in 0..n2 -> &x[MINDEX2(n1,n2,n1-i1,i2)] ~> "
      "Cell");
  complex_access_ok(x, n1, n2);
}

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

// Should also work when we can match the * with a specified c
void complex_access_ok_2(float *x, int n1, int n2, int c_callee) {
  __modifies("for i2 in 0..n2 -> &x[MINDEX2(n1,n2,(c_callee + c_callee) / "
             "2,i2)] ~> Cell");
  __admitted();
}

void complex_access_ok_caller_2(float *x, int n1, int n2) {
  __modifies("for i1 in 0..n1 -> for i2 in 0..n2 -> &x[MINDEX2(n1,n2,(i1+i1) / "
             "2,i2)] ~> "
             "Cell");
  const int c = 3;
  complex_access_ok_2(x, n1, n2, c);
}

// // // generic case
// int f(const int a,const int b) {
//   __pure();
//   __admitted();
//   return (a + b) / 2;
// }

// void complex_access_generic(float *x, int n1, int n2, int d) {
//   __reads("for i1 in 0..n1 -> &x[MINDEX2(n1,n2,f(d,d),2)] ~> Cell");
//   __admitted();
// }

// void complex_access_generic_caller(float *x, int n1, int n2) {
//   __reads(
//       "for i1 in 0..n1 -> for i2 in 0..n2 -> &x[MINDEX2(n1,n2,f(i1,i1),i2)]~> Cell");
//   const int c = 3;
//   complex_access_generic(x, n1, n2, c);
// }
// void complex_access_abort(float *x, int n1, int n2) {
//   __reads("for i2 in 0..n2 -> &x[MINDEX2(n1,n2,4,n2)] ~> Cell");
//   for (int i2 = 0; i2 < n2; i2++) {
//     float a = x[MINDEX2(n1, n2, 4, i2)];
//   }
// }

// void complex_access_abort_caller(float *x, int n1, int n2) {
//   __reads(
//       "for i1 in 0..n2 -> for i2 in 0..n2 -> &x[MINDEX2(n1,n2,n1-i1,i2)] ~> "
//       "Cell");
//   complex_access_abort_caller(x, n1, n2);
// }
// // Renaming : Iterator might not have the same name

void rename_and_focus(float *x, int n1, int n2) {
  __modifies("for j in 0..n2  -> &x[MINDEX2(n1,n2,2,j)] ~> Cell");
  __admitted();
}

void rename_and_focus_caller(float *x, int n1, int n2) {
  __modifies(
      "for i1 in 0..n1 -> for i2 in 0..n2 -> &x[MINDEX2(n1,n2,i1,i2)] ~> Cell");
  rename_and_focus(x, n1, n2);
}

// // different_order : Stars mights be in a different order between the caller
// and
// // the callee (even without focus), but it should work

// void different_order(float *x, int n1, int n2, int n3) {
//   __modifies("for i2 in 0..n2 -> for i3 in 0..n3 -> for i1 in 0..n1 -> "
//           "&x[MINDEX3(n1,n2,n3,i1,i2,i3)] ~> Cell");
//   __admitted();
// }

// void different_order_caller(float *x, int n1, int n2, int n3) {
//   __modifies("for i1 in 0..n1 -> for i2 in 0..n2 -> for i3 in 0..n3 -> "
//           "&x[MINDEX3(n1,n2,n3,i1,i2,i3)] ~> Cell");
//   different_order(x, n1, n2, n3);
// }

// // generic with different order : can be any HProp
// void generic_different_order(float *x, int n1, int n2, int n3) {
//   __requires("items: int * int * int -> HProp");
//   __reads("for i2 in 0..n2 -> for i3 in 0..n3 -> for i1 in 0..n1 -> "
//           " items(i1,i2,i3) ~> Cell");
// }
// void generic_different_order_caller(float *x, int n1, int n2, int n3) {
//   __requires("items: int * int * int -> HProp");
//   __reads("for i1 in 0..n1 -> for i2 in 0..n2 -> for i3 in 0..n3 -> "
//           " items(i1,i2,i3) ~> Cell");
//   generic_different_order(x, n1, n2, n3);
// }

// void loop_basic(float *x, int n1, int n2) {
//   __reads(
//       "for i1 in 0..n1 -> for i2 in 0..n2 -> &x[MINDEX2(n1,n2,i1,i2)] ~>
//       Cell");
//   for (int i1 = 0; i1 < n1; i1++) {
//     for (int i2 = 0; i2 < n2; i2++) {
//       float a = x[MINDEX2(n1, n2, i1, i2)];
//     }
//   }
// }

// void loop_multi_focus(float *x, int n1, int n2, int n3) {
//   __modifies("x ~> Matrix3(n1,n2,n3)");
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
// // const a = ghost_begin(focus1, "for i in 0..n1 -> for i2 in 0..n2 -> for i3
// in
// // 0..n3 -> &x[MINDEX3(n1,n2,n3,i1,i2,i3)] ~>Cell,   i: 1") ; <-- This ghost
// can
// // go outside the loops, const b = ghost_begin(focus1, "for i in 0 ..n2 ->
// for
// // i3 in 0..n3 -> &x[MINDEX3(n1,n2,n3,1,i2,i3)] ~>Cell, i := i2"); <-- This
// one
// // can go in between const c = ghost_begin(focus1, "for i in 0..n3 ->
// // &x[MINDEX3(n1,n2,n3,1,i2,i3)] ~>Cell, i := i3"); < -- This one must stay
// here
// //       float a = x[MINDEX3(n1, n2, n3, 1, i2, i3)];
// //     }
// //   }
// // }

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
