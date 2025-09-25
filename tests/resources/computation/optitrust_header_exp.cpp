#include <optitrust_common.h>

__ghost_ret matrix2_focus() {
  __requires("T: Type");
  __requires("matrix: ptr(T)");
  __requires("i: int");
  __requires("j: int");
  __requires("m: int");
  __requires("n: int");
  __requires("bound_check_i: in_range(i, 0..m)");
  __requires("bound_check_j: in_range(j, 0..n)");
  __consumes("matrix ~> Matrix2(m, n)");
  __produces(
      "Wand(&matrix[MINDEX2(m, n, i, j)] ~> Cell, matrix ~> Matrix2(m, n))");
  __produces("&matrix[MINDEX2(m, n, i, j)] ~> Cell");
  __ghost(group_focus, "i := i, bound_check := bound_check_i");
  __ghost(group_focus, "i := j, bound_check := bound_check_j");
  __ghost(wand_simplify);
}

__ghost_ret matrix2_unfocus() {
  __reverts(matrix2_focus);
  __ghost(close_wand);
}

__ghost_ret matrix1_focus() {
  __requires("T: Type");
  __requires("matrix: ptr(T)");
  __requires("i: int");
  __requires("n: int");
  __requires("bound_check: in_range(i, 0..n)");
  __consumes("matrix ~> Matrix1(n)");
  __produces("Wand(&matrix[MINDEX1(n, i)] ~> Cell, matrix ~> Matrix1(n))");
  __produces("&matrix[MINDEX1(n, i)] ~> Cell");
  __ghost(group_focus, "i := i, bound_check := bound_check");
}

__ghost_ret matrix1_unfocus() {
  __reverts(matrix1_focus);
  __ghost(close_wand);
}

__ghost_ret ro_matrix1_focus() {
  __requires("T: Type");
  __requires("matrix: ptr(T)");
  __requires("i: int");
  __requires("n: int");
  __requires("f: _Fraction");
  __requires("bound_check: in_range(i, 0..n)");
  __consumes("_RO(f, matrix ~> Matrix1(n))");
  __produces(
      "Wand(_RO(f, &matrix[MINDEX1(n, i)] ~> Cell), _RO(f, matrix ~> "
      "Matrix1(n)))");
  __produces("_RO(f, &matrix[MINDEX1(n, i)] ~> Cell)");
  __admitted();
  __ghost(ro_group_focus, "f := f, i := i, bound_check := bound_check");
}

__ghost_ret ro_matrix1_unfocus() {
  __reverts(ro_matrix1_focus);
  __admitted();
  __ghost(close_wand);
}

__ghost_ret ro_matrix2_focus() {
  __requires("T: Type");
  __requires("matrix: ptr(T)");
  __requires("i: int");
  __requires("j: int");
  __requires("m: int");
  __requires("n: int");
  __requires("f: _Fraction");
  __requires("bound_check_i: in_range(i, 0..m)");
  __requires("bound_check_j: in_range(j, 0..n)");
  __consumes("_RO(f, matrix ~> Matrix2(m, n))");
  __produces(
      "Wand(_RO(f, &matrix[MINDEX2(m, n, i, j)] ~> Cell), _RO(f, matrix ~> "
      "Matrix2(m, n)))");
  __produces("_RO(f, &matrix[MINDEX2(m, n, i, j)] ~> Cell)");
  __ghost(ro_group_focus, "f := f, i := i, bound_check := bound_check_i");
  __ghost(ro_group_focus, "f := f, i := j, bound_check := bound_check_j");
  __ghost(wand_simplify);
}

__ghost_ret ro_matrix2_unfocus() {
  __reverts(ro_matrix2_focus);
  __admitted();
  __ghost(close_wand);
}

__ghost_ret ro_matrix3_focus() {
  __requires("T: Type");
  __requires("matrix: ptr(T)");
  __requires("i: int");
  __requires("j: int");
  __requires("k: int");
  __requires("m: int");
  __requires("n: int");
  __requires("o: int");
  __requires("f: _Fraction");
  __requires("bound_check_i: in_range(i, 0..m)");
  __requires("bound_check_j: in_range(j, 0..n)");
  __requires("bound_check_k: in_range(k, 0..o)");
  __consumes("_RO(f, matrix ~> Matrix3(m, n, o))");
  __produces(
      "Wand(_RO(f, &matrix[MINDEX3(m, n, o, i, j, k)] ~> Cell), _RO(f, matrix "
      "~> Matrix3(m, n, o)))");
  __produces("_RO(f, &matrix[MINDEX3(m, n, o, i, j, k)] ~> Cell)");
  __ghost(ro_group_focus, "f := f, i := i, bound_check := bound_check_i");
  __ghost(ro_group_focus, "f := f, i := j, bound_check := bound_check_j");
  __ghost(ro_group_focus, "f := f, i := k, bound_check := bound_check_k");
  __ghost(wand_simplify);
  __ghost(wand_simplify);
}

__ghost_ret ro_matrix3_unfocus() {
  __reverts(ro_matrix3_focus);
  __admitted();
  __ghost(close_wand);
}

void MATRIX1_COPY_int(int* dest, int* src, int length) {
  __writes("dest ~> Matrix1(length)");
  __reads("src ~> Matrix1(length)");
  __admitted();
  memcpy(dest, src, length * sizeof(int));
}

void MATRIX2_COPY_int(int* dest, int* src, int n1, int n2) {
  __writes("dest ~> Matrix2(n1, n2)");
  __reads("src ~> Matrix2(n1, n2)");
  __admitted();
  memcpy(dest, src, n1 * n2 * sizeof(int));
}

void MATRIX3_COPY_int(int* dest, int* src, int n1, int n2, int n3) {
  __writes("dest ~> Matrix3(n1, n2, n3)");
  __reads("src ~> Matrix3(n1, n2, n3)");
  __admitted();
  memcpy(dest, src, n1 * n2 * n3 * sizeof(int));
}

void MATRIX1_COPY_float(float* dest, float* src, int length) {
  __writes("dest ~> Matrix1(length)");
  __reads("src ~> Matrix1(length)");
  __admitted();
  memcpy(dest, src, length * sizeof(float));
}

void MATRIX2_COPY_float(float* dest, float* src, int n1, int n2) {
  __writes("dest ~> Matrix2(n1, n2)");
  __reads("src ~> Matrix2(n1, n2)");
  __admitted();
  memcpy(dest, src, n1 * n2 * sizeof(float));
}

void MATRIX3_COPY_float(float* dest, float* src, int n1, int n2, int n3) {
  __writes("dest ~> Matrix3(n1, n2, n3)");
  __reads("src ~> Matrix3(n1, n2, n3)");
  __admitted();
  memcpy(dest, src, n1 * n2 * n3 * sizeof(float));
}

void MATRIX1_COPY_double(double* dest, double* src, int length) {
  __writes("dest ~> Matrix1(length)");
  __reads("src ~> Matrix1(length)");
  __admitted();
  memcpy(dest, src, length * sizeof(double));
}

void MATRIX2_COPY_double(double* dest, double* src, int n1, int n2) {
  __writes("dest ~> Matrix2(n1, n2)");
  __reads("src ~> Matrix2(n1, n2)");
  __admitted();
  memcpy(dest, src, n1 * n2 * sizeof(double));
}

void MATRIX3_COPY_double(double* dest, double* src, int n1, int n2, int n3) {
  __writes("dest ~> Matrix3(n1, n2, n3)");
  __reads("src ~> Matrix3(n1, n2, n3)");
  __admitted();
  memcpy(dest, src, n1 * n2 * n3 * sizeof(double));
}

void MATRIX1_MEMSET_int(int* dest, int value, int length) {
  __writes("dest ~> Matrix1(length)");
  __admitted();
  memset(dest, value, length * sizeof(int));
}

void MATRIX2_MEMSET_int(int* dest, int value, int n1, int n2) {
  __writes("dest ~> Matrix2(n1, n2)");
  __admitted();
  memset(dest, value, n1 * n2 * sizeof(int));
}

void MATRIX3_MEMSET_int(int* dest, int value, int n1, int n2, int n3) {
  __writes("dest ~> Matrix3(n1, n2, n3)");
  __admitted();
  memset(dest, value, n1 * n2 * n3 * sizeof(int));
}

void MATRIX1_MEMSET_float(float* dest, float value, int length) {
  __writes("dest ~> Matrix1(length)");
  __admitted();
  memset(dest, value, length * sizeof(float));
}

void MATRIX2_MEMSET_float(float* dest, float value, int n1, int n2) {
  __writes("dest ~> Matrix2(n1, n2)");
  __admitted();
  memset(dest, value, n1 * n2 * sizeof(float));
}

void MATRIX3_MEMSET_float(float* dest, float value, int n1, int n2, int n3) {
  __writes("dest ~> Matrix3(n1, n2, n3)");
  __admitted();
  memset(dest, value, n1 * n2 * n3 * sizeof(float));
}

void MATRIX1_MEMSET_double(double* dest, double value, int length) {
  __writes("dest ~> Matrix1(length)");
  __admitted();
  memset(dest, value, length * sizeof(double));
}

void MATRIX2_MEMSET_double(double* dest, double value, int n1, int n2) {
  __writes("dest ~> Matrix2(n1, n2)");
  __admitted();
  memset(dest, value, n1 * n2 * sizeof(double));
}

void MATRIX3_MEMSET_double(double* dest, double value, int n1, int n2, int n3) {
  __writes("dest ~> Matrix3(n1, n2, n3)");
  __admitted();
  memset(dest, value, n1 * n2 * n3 * sizeof(double));
}

__ghost_ret matrix2_span_shift() {
  __requires("T: Type");
  __requires("matrix: ptr(T)");
  __requires("n1: int");
  __requires("n2: int");
  __requires("a: int");
  __requires("b: int");
  __consumes(
      "for i in a..b -> for j in 0..n2 -> &matrix[MINDEX2(n1, n2, i, j)] ~> "
      "Cell");
  __produces("&matrix[a * n2] ~> Matrix2(b - a, n2)");
  __admitted();
}

__ghost_ret matrix3_span_shift() {
  __requires("T: Type");
  __requires("matrix: ptr(T)");
  __requires("n1: int");
  __requires("n2: int");
  __requires("n3: int");
  __requires("a: int");
  __requires("b: int");
  __consumes(
      "for i1 in a..b -> for i2 in 0..n2 -> for i3 in 0..n3 -> "
      "&matrix[MINDEX3(n1, n2, n3, i1, i2, i3)] ~> Cell");
  __produces("&matrix[a * n2 * n3] ~> Matrix3(b - a, n2, n3)");
  __admitted();
}

uint16_t reduce_spe1(int start, int stop, uint8_t* input, int n, int m, int j) {
  __requires("check_range: is_subrange(start..stop, 0..n)");
  __requires("bound_check: in_range(j, 0..m)");
  __reads("input ~> Matrix2(n, m)");
  __admitted();
  uint16_t s = (uint16_t)0;
  for (int i = start; i < stop; i++) {
    __smodifies("&s ~> Cell");
    __sreads("input ~> Matrix2(n, m)");
    __ghost(in_range_extend, "x := i, r1 := start..stop, r2 := 0..n");
    const __ghost_fn focus =
        __ghost_begin(ro_matrix2_focus, "matrix := input, i := i, j := j");
    s += (uint16_t)input[MINDEX2(n, m, i, j)];
    __ghost_end(focus);
  }
  return s;
}

float get_max(float* x, int n) {
  __reads("x ~> Matrix1(n)");
  float max_value = 0.f;
  for (int i = 0; i < n; i++) {
    __strict();
    __smodifies("&max_value ~> Cell");
    __sreads("x ~> Matrix1(n)");
    const __ghost_fn f = __ghost_begin(ro_matrix1_focus, "matrix := x, i := i");
    max_value = maxf(x[MINDEX1(n, i)], max_value);
    __ghost_end(f);
  }
  return max_value;
}
