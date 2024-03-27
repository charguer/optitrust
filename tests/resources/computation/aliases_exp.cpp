#include <optitrust.h>

void array_copy_ref_alias(float* A, float* B, int n) {
  __modifies("B ~> Matrix1(n)");
  __reads("A ~> Matrix1(n)");
  for (int i = 0; i < n; ++i) {
    __strict();
    __modifies("&B[MINDEX1(n, i)] ~> Cell");
    __reads("&A[MINDEX1(n, i)] ~> Cell");
    float& a = A[MINDEX1(n, i)];
    float& b = B[MINDEX1(n, i)];
    b = a;
  }
}

void array_copy_ptr_alias(float* A, float* B, int n) {
  __modifies("B ~> Matrix1(n)");
  __reads("A ~> Matrix1(n)");
  for (int i = 0; i < n; ++i) {
    __strict();
    __modifies("&B[MINDEX1(n, i)] ~> Cell");
    __reads("&A[MINDEX1(n, i)] ~> Cell");
    float* const a = &A[MINDEX1(n, i)];
    float* const b = &B[MINDEX1(n, i)];
    *b = *a;
  }
}

void array_copy_index_alias(float* A, float* B, int n) {
  __modifies("B ~> Matrix1(n)");
  __reads("A ~> Matrix1(n)");
  for (int i = 0; i < n; ++i) {
    __strict();
    __modifies("&B[MINDEX1(n, i)] ~> Cell");
    __reads("&A[MINDEX1(n, i)] ~> Cell");
    const int j = i;
    B[MINDEX1(n, j)] = A[MINDEX1(n, j)];
  }
}

void array_copy_alias_on_alias(float* A, float* B, int n) {
  __modifies("B ~> Matrix1(n)");
  __reads("A ~> Matrix1(n)");
  float* const C = A;
  for (int i = 0; i < n; ++i) {
    __strict();
    __modifies("&B[MINDEX1(n, i)] ~> Cell");
    __reads("&A[MINDEX1(n, i)] ~> Cell");
    float& a = C[MINDEX1(n, i)];
    float& b1 = B[MINDEX1(n, i)];
    float& b2 = b1;
    b2 = a;
  }
}

void dummy_modifies(int* p) { __modifies("p ~> Cell"); }

void bidirectional_alias() {
  __pure();
  int x = 0;
  int& y = x;
  dummy_modifies(&y);
  x++;
}

void eq_to_alias(float* A, int n) {
  __requires("__is_eq(n, 1024)");
  __modifies("A ~> Matrix1(n)");
  __ghost(assert_alias, "x := n");
  for (int i = 0; i < 1024; ++i) {
    __strict();
    __modifies("&A[MINDEX1(1024, i)] ~> Cell");
    A[MINDEX1(1024, i)] = 0;
  }
}
