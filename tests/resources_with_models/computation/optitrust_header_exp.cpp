#include <optitrust_common.h>

extern __ghost_unit __OPTITRUST_ENABLE_MODELS;

__ghost_ret ro_matrix1_focus() {
  __requires("T: Type");
  __requires("matrix: ptr(T)");
  __requires("i: int");
  __requires("n: int");
  __requires("MT: MemType");
  __requires("M: int -> T");
  __requires("f: _Fraction");
  __requires("bound_check: in_range(i, 0..n)");
  __consumes("_RO(f, matrix ~> Matrix1Of(n, MT, M))");
  __produces(
      "Wand(_RO(f, &matrix[MINDEX1(n, i)] ~~>[MT] M(i)), _RO(f, matrix ~> "
      "Matrix1Of(n, MT, M)))");
  __produces("_RO(f, &matrix[MINDEX1(n, i)] ~~>[MT] M(i))");
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
  __requires("MT: MemType");
  __requires("M: int * int -> T");
  __requires("f: _Fraction");
  __requires("bound_check_i: in_range(i, 0..m)");
  __requires("bound_check_j: in_range(j, 0..n)");
  __consumes("_RO(f, matrix ~> Matrix2Of(m, n, MT, M))");
  __produces(
      "Wand(_RO(f, &matrix[MINDEX2(m, n, i, j)] ~~>[MT] M(i, j)), _RO(f, "
      "matrix ~> Matrix2Of(m, n, MT, M)))");
  __produces("_RO(f, &matrix[MINDEX2(m, n, i, j)] ~~>[MT] M(i, j))");
  __admitted();
  __ghost(ro_group_focus, "f := f, i := i, bound_check := bound_check_i");
  __ghost(ro_group_focus, "f := f, i := j, bound_check := bound_check_j");
  __ghost(wand_simplify);
}

__ghost_ret ro_matrix2_unfocus() {
  __reverts(ro_matrix2_focus);
  __admitted();
  __ghost(close_wand);
}

void MATRIX1_COPY_int(int* dest, int* src, int length) {
  __requires("model: int -> int");
  __writes("dest ~> Matrix1(length, model)");
  __reads("src ~> Matrix1(length, model)");
  __admitted();
  memcpy(dest, src, length * sizeof(int));
}

void MATRIX2_COPY_int(int* dest, int* src, int n1, int n2) {
  __requires("model: int * int -> int");
  __writes("dest ~> Matrix2(n1, n2, model)");
  __reads("src ~> Matrix2(n1, n2, model)");
  __admitted();
  memcpy(dest, src, n1 * n2 * sizeof(int));
}

void MATRIX3_COPY_int(int* dest, int* src, int n1, int n2, int n3) {
  __requires("model: int * int * int -> int");
  __writes("dest ~> Matrix3(n1, n2, n3, model)");
  __reads("src ~> Matrix3(n1, n2, n3, model)");
  __admitted();
  memcpy(dest, src, n1 * n2 * n3 * sizeof(int));
}

void MATRIX1_COPY_float(float* dest, float* src, int length) {
  __requires("model: int -> float");
  __writes("dest ~> Matrix1(length, model)");
  __reads("src ~> Matrix1(length, model)");
  __admitted();
  memcpy(dest, src, length * sizeof(float));
}

void MATRIX2_COPY_float(float* dest, float* src, int n1, int n2) {
  __requires("model: int * int -> float");
  __writes("dest ~> Matrix2(n1, n2, model)");
  __reads("src ~> Matrix2(n1, n2, model)");
  __admitted();
  memcpy(dest, src, n1 * n2 * sizeof(float));
}

void MATRIX3_COPY_float(float* dest, float* src, int n1, int n2, int n3) {
  __requires("model: int * int * int -> float");
  __writes("dest ~> Matrix3(n1, n2, n3, model)");
  __reads("src ~> Matrix3(n1, n2, n3, model)");
  __admitted();
  memcpy(dest, src, n1 * n2 * n3 * sizeof(float));
}

void MATRIX1_COPY_double(double* dest, double* src, int length) {
  __requires("model: int -> double");
  __writes("dest ~> Matrix1(length, model)");
  __reads("src ~> Matrix1(length, model)");
  __admitted();
  memcpy(dest, src, length * sizeof(double));
}

void MATRIX2_COPY_double(double* dest, double* src, int n1, int n2) {
  __requires("model: int * int -> double");
  __writes("dest ~> Matrix2(n1, n2, model)");
  __reads("src ~> Matrix2(n1, n2, model)");
  __admitted();
  memcpy(dest, src, n1 * n2 * sizeof(double));
}

void MATRIX3_COPY_double(double* dest, double* src, int n1, int n2, int n3) {
  __requires("model: int * int * int -> double");
  __writes("dest ~> Matrix3(n1, n2, n3, model)");
  __reads("src ~> Matrix3(n1, n2, n3, model)");
  __admitted();
  memcpy(dest, src, n1 * n2 * n3 * sizeof(double));
}

__ghost_ret matrix2_span_shift() {
  __requires("T: Type");
  __requires("matrix: ptr(T)");
  __requires("n1: int");
  __requires("n2: int");
  __requires("a: int");
  __requires("b: int");
  __requires("M: int * int -> T");
  __consumes(
      "for i in a..b -> for j in 0..n2 -> &matrix[MINDEX2(n1, n2, i, j)] ~~> "
      "M(i, j)");
  __produces(
      "for i in 0..(b - a) -> for j in 0..n2 -> &(&matrix[a * n2])[MINDEX2(b - "
      "a, n2, i, j)] ~~> M(i + a, j)");
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
  __requires("M: int * int * int -> T");
  __consumes(
      "for i1 in a..b -> for i2 in 0..n2 -> for i3 in 0..n3 -> "
      "&matrix[MINDEX3(n1, n2, n3, i1, i2, i3)] ~~> M(i1, i2, i3)");
  __produces(
      "for i1 in 0..(b - a) -> for i2 in 0..n2 -> for i3 in 0..n3 -> "
      "&(&matrix[a * n2 * n3])[MINDEX3(b - a, n2, n3, i1, i2, i3)] ~~> M(i1 + "
      "a, i2, i3)");
  __admitted();
}

__ghost(assert_inhabited, "x := arbitrary(int * int * (int -> int) -> int)",
        "reduce_int_sum <- x");

__ghost(assert_prop,
        "proof := admit(forall (n: int) (f: int -> int) -> __is_true(0 == "
        "reduce_int_sum(n, n, f)))",
        "reduce_int_sum_empty <- proof");

__ghost(
    assert_prop,
    "proof := admit(forall (a: int) (b: int) (f: int -> int) (_: __is_true(b "
    ">= a)) (bp1: int) (_: __is_true(bp1 == b + 1)) -> "
    "__is_true(reduce_int_sum(a, b, f) + f(b) == reduce_int_sum(a, bp1, f)))",
    "reduce_int_sum_add_right <- proof");

__ghost(
    assert_prop,
    "proof := admit(forall (a: int) (b: int) (f: int -> int) (_: __is_true(b > "
    "a)) (ap1: int) (_: __is_true(ap1 == a + 1)) -> "
    "__is_true(reduce_int_sum(a, b, f) - f(a) == reduce_int_sum(ap1, b, f)))",
    "reduce_int_sum_sub_left <- proof");

__ghost(assert_prop,
        "proof := admit(forall (a: int) (b: int) (ap1: int) (bp1: int) (f: int "
        "-> int) (_: __is_true(b >= a)) (_: __is_true(ap1 == a + 1)) (_: "
        "__is_true(bp1 == b + 1)) -> __is_true(reduce_int_sum(a, b, f) + (f(b) "
        "- f(a)) == reduce_int_sum(ap1, bp1, f)))",
        "reduce_int_sum_slide <- proof");
