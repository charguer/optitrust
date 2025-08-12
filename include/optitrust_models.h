#ifndef OPTITRUST_H
#define OPTITRUST_H

#include <optitrust_common.h>

extern __ghost_unit __OPTITRUST_ENABLE_MODELS;

/* ---- Matrix Ghosts ---- */

/*__GHOST(matrix2_focus)  {
  __requires("T: Type, matrix: ptr(T), i: int, j: int, m: int, n: int");
  __requires("bound_check_i: in_range(i, 0..m)");
  __requires("bound_check_j: in_range(j, 0..n)");
  __consumes("matrix ~> Matrix2(m, n)");
  __produces("Wand(&matrix[MINDEX2(m, n, i, j)] ~> Cell, matrix ~> Matrix2(m, n)), &matrix[MINDEX2(m, n, i, j)] ~> Cell");

  __ghost(group_focus, "i := i, bound_check := bound_check_i");
  __ghost(group_focus, "i := j, bound_check := bound_check_j");
  __ghost(wand_simplify);
}

__GHOST(matrix2_unfocus) {
  __reverts(matrix2_focus);

  __ghost(close_wand);
}

__GHOST(matrix1_focus) {
  __requires("T: Type, matrix: ptr(T), i: int, n: int");
  __requires("bound_check: in_range(i, 0..n)");
  __consumes("matrix ~> Matrix1(n)");
  __produces("Wand(&matrix[MINDEX1(n, i)] ~> Cell, matrix ~> Matrix1(n)), &matrix[MINDEX1(n, i)] ~> Cell");

  __ghost(group_focus, "i := i, bound_check := bound_check");
}

__GHOST(matrix1_unfocus) {
  __reverts(matrix1_focus);

  __ghost(close_wand);
}*/

__GHOST(ro_matrix1_focus) {
  __requires("T: Type, matrix: ptr(T), i: int, n: int, M: int -> T, f: _Fraction");
  __requires("bound_check: in_range(i, 0..n)");
  __consumes("_RO(f, matrix ~> Matrix1(n, M))");
  __produces("Wand(_RO(f, &matrix[MINDEX1(n, i)] ~~> M(i)), _RO(f, matrix ~> Matrix1(n, M))), _RO(f, &matrix[MINDEX1(n, i)] ~~> M(i))");
  __admitted(); // for efficiency
  __ghost(ro_group_focus, "f := f, i := i, bound_check := bound_check");
}

__GHOST(ro_matrix1_unfocus) {
  __reverts(ro_matrix1_focus);
  __admitted(); // for efficiency
  __ghost(close_wand);
}

__GHOST(ro_matrix2_focus) {
  __requires("T: Type, matrix: ptr(T), i: int, j: int, m: int, n: int, M: int * int -> T, f: _Fraction");
  __requires("bound_check_i: in_range(i, 0..m)");
  __requires("bound_check_j: in_range(j, 0..n)");
  __consumes("_RO(f, matrix ~> Matrix2(m, n, M))");
  __produces("Wand(_RO(f, &matrix[MINDEX2(m, n, i, j)] ~~> M(i, j)), _RO(f, matrix ~> Matrix2(m, n, M))), _RO(f, &matrix[MINDEX2(m, n, i, j)] ~~> M(i, j))");
  __admitted(); // for efficiency
  __ghost(ro_group_focus, "f := f, i := i, bound_check := bound_check_i");
  __ghost(ro_group_focus, "f := f, i := j, bound_check := bound_check_j");
  __ghost(wand_simplify);
}

__GHOST(ro_matrix2_unfocus) {
  __reverts(ro_matrix2_focus);
  __admitted(); // for efficiency
  __ghost(close_wand);
}

/* ---- Definition of matrix memcpy ---- */

#define DEFINE_MATRIX_COPY(T) \
  inline void MATRIX1_COPY_##T(T* dest, T* src, int length) { \
    __requires("model: int -> " #T); \
    __reads("src ~> Matrix1(length, model)"); \
    __writes("dest ~> Matrix1(length, model)"); \
    __admitted(); \
    memcpy(dest, src, length * sizeof(T)); \
  } \
  inline void MATRIX2_COPY_##T(T* dest, T* src, int n1, int n2) { \
    __requires("model: int * int -> " #T); \
    __reads("src ~> Matrix2(n1, n2, model)"); \
    __writes("dest ~> Matrix2(n1, n2, model)"); \
    __admitted(); \
    memcpy(dest, src, n1 * n2 * sizeof(T)); \
  } \
  inline void MATRIX3_COPY_##T(T* dest, T* src, int n1, int n2, int n3) { \
    __requires("model: int * int * int -> " #T); \
    __reads("src ~> Matrix3(n1, n2, n3, model)"); \
    __writes("dest ~> Matrix3(n1, n2, n3, model)"); \
    __admitted(); \
    memcpy(dest, src, n1 * n2 * n3 * sizeof(T)); \
  }

DEFINE_MATRIX_COPY(int)
DEFINE_MATRIX_COPY(float)
DEFINE_MATRIX_COPY(double)

// span_shift unused for now: this pattern allows copy of an arbitrary span of a matrix
// TODO: uninit and ro variants

__GHOST(matrix2_span_shift) {
  __requires("T: Type, matrix: ptr(T), n1: int, n2: int, a: int, b: int, M: int * int -> T");
  __consumes("for i in a..b -> for j in 0..n2 -> &matrix[MINDEX2(n1, n2, i, j)] ~~> M(i, j)");
  __produces("for i in 0..(b-a) -> for j in 0..n2 -> &(&matrix[a*n2])[MINDEX2(b-a, n2, i, j)] ~~> M(i+a, j)");
  __admitted();
}

__GHOST(matrix3_span_shift) {
  __requires("T: Type, matrix: ptr(T), n1: int, n2: int, n3: int, a: int, b: int, M: int * int * int -> T");
  __consumes("for i1 in a..b -> for i2 in 0..n2 -> for i3 in 0..n3 -> &matrix[MINDEX3(n1, n2, n3, i1, i2, i3)] ~~> M(i1, i2, i3)");
  __produces("for i1 in 0..(b-a) -> for i2 in 0..n2 -> for i3 in 0..n3 -> &(&matrix[a*n2*n3])[MINDEX3(b-a, n2, n3, i1, i2, i3)] ~~> M(i1+a, i2, i3)");
  __admitted();
}

/* ---- Algorithmic Functions ---- */

__DECL(reduce_int_sum, "int * int * (int -> int) -> int");
__AXIOM(reduce_int_sum_empty, "forall (n : int) (f: int -> int) -> 0 = reduce_int_sum(n, n, f)");
// __AXIOM(reduce_int_sum_add_right, "forall (a: int) (b: int) (f: int -> int) (_: b >= a) -> reduce_int_sum(a, b, f) + f(b) = reduce_int_sum(a, b + 1, f)");
// FIXME: for now more robust to arith rewrites
__AXIOM(reduce_int_sum_add_right, "forall (a: int) (b: int) (f: int -> int) (_: b >= a) (bp1 : int) (_: bp1 = b + 1) -> reduce_int_sum(a, b, f) + f(b) = reduce_int_sum(a, bp1, f)");
__AXIOM(reduce_int_sum_sub_left, "forall (a: int) (b: int) (f: int -> int) (_: b > a) (ap1 : int) (_: ap1 = a + 1) -> reduce_int_sum(a, b, f) - f(a) = reduce_int_sum(ap1, b, f)");

__AXIOM(reduce_int_sum_slide, "forall (a: int) (b: int) (ap1: int) (bp1: int) (f: int -> int) (_: b >= a) (_: ap1 = a + 1) (_: bp1 = b + 1) -> reduce_int_sum(a, b, f) + (f(bp1) - f(a)) = reduce_int_sum(ap1, bp1, f)");
/*
  "forall (a: int) (b: int) (ap1: int) (bp1: int) (f: int -> int)  (b_geq_a: b >= a) (ap1_eq: ap1 = a + 1) (bp1_eq: bp1 = b + 1) -> rw reduce_int_sum_add_right(a, b, f, b_geq_a, bp1_eq);
  rw reduce_int_sum_sub_left(a, bp1, f, _, ap1_eq)");
*/

// DEF

/* TODO: generalize to any monoid/group, see doc/sliding-window.md */
// template<typename T, typename ST>
// NOTE: using 'int' as 'Z' to avoid overflow questions
/*
inline int reduce_spe1(int start, int stop, int* input, int n, int m, int j) {
  __requires("check_range: is_subrange(start..stop, 0..n)");
  __requires("bound_check: in_range(j, 0..m)");
  __requires("M: int * int -> int"); // FIXME: uint8_t ???
  __reads("input ~> Matrix2(n, m, M)");
  __ensures("_Res = reduce_int_sum(stop - start, fun k -> M(k + start, j))");
  __admitted(); // NOT NECESSARY, BUT FASTER
  // __reads("for k in 0..n -> &input[MINDEX2(n, m, k, j)] ~> Cell");

  int s = 0;
  for (int i = start; i < stop; i++) {
    // __spreserves("&s ~> reduce_int_sum(i - start, fun k -> M(k + start, j))");
    // __sreads("input ~> Matrix2(n, m, M)");

    // __ghost(in_range_extend, "i, start..stop, 0..n");
    // __GHOST_BEGIN(focus, ro_matrix2_focus, "input, i, j");
    s += input[MINDEX2(n, m, i, j)];
    // __GHOST_END(focus);
  }
  return s;
}
*/
#endif
