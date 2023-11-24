#include <optitrust.h>

void array_copy_ref_alias(float* A, float* B, int n) {
    __reads("A ~> Matrix1(n)");
    __modifies("B ~> Matrix1(n)");
    for (int i = 0; i < n; ++i) {
        __reads("&A[MINDEX1(n, i)] ~> Cell");
        __modifies("&B[MINDEX1(n, i)] ~> Cell");
        float& a = A[MINDEX1(n, i)];
        float& b = B[MINDEX1(n, i)];
        b = a;
    }
}

void array_copy_ptr_alias(float* A, float* B, int n) {
    __reads("A ~> Matrix1(n)");
    __modifies("B ~> Matrix1(n)");
    for (int i = 0; i < n; ++i) {
        __reads("&A[MINDEX1(n, i)] ~> Cell");
        __modifies("&B[MINDEX1(n, i)] ~> Cell");
        float* const a = &A[MINDEX1(n, i)];
        float* const b = &B[MINDEX1(n, i)];
        *b = *a;
    }
}

void array_copy_index_alias(float* A, float* B, int n) {
    __reads("A ~> Matrix1(n)");
    __modifies("B ~> Matrix1(n)");
    for (int i = 0; i < n; ++i) {
        __reads("&A[MINDEX1(n, i)] ~> Cell");
        __modifies("&B[MINDEX1(n, i)] ~> Cell");
        const int j = i;
        B[MINDEX1(n, j)] = A[MINDEX1(n, j)];
    }
}

void array_copy_alias_on_alias(float* A, float* B, int n) {
    __reads("A ~> Matrix1(n)");
    __modifies("B ~> Matrix1(n)");
    float* const C = A;
    for (int i = 0; i < n; ++i) {
        __reads("&A[MINDEX1(n, i)] ~> Cell");
        __modifies("&B[MINDEX1(n, i)] ~> Cell");
        float& a = C[MINDEX1(n, i)];
        float& b1 = B[MINDEX1(n, i)];
        float& b2 = b1;
        b2 = a;
    }
}

// Could be useful to test if aliases can be kept by evar resolution

void dummy_modifies(int* p) {
  __modifies("p ~> Cell");
}

void bidirectional_alias() {
  int x = 0;
  int& y = x;
  dummy_modifies(&y);
  x++;
}
