#ifndef OPTITRUST_H
#define OPTITRUST_H

#include <optitrust_common.h>

extern __ghost_unit __OPTITRUST_ENABLE_MODELS;

/* ---- Definition of matrix memcpy ---- */

#define DEFINE_MMEMCPY(T) \
  inline void MMEMCPY_##T(T* dest, int d_offset, T* src, int s_offset, int length) { \
    __requires("d_end: int, s_end: int, d_all: int, s_all: int"); \
    __requires("check_d_size: d_end = d_offset + length"); \
    __requires("check_s_size: s_end = s_offset + length"); \
    __requires("model: int -> " #T); \
    __reads("for k in s_offset..s_end -> &src[MINDEX1(s_all, k)] ~~> model(k)"); \
    __writes("for k in d_offset..d_end -> &dest[MINDEX1(d_all, k)] ~~> model(k - d_offset + s_offset)"); \
    __admitted(); \
    memcpy(&dest[d_offset], &src[s_offset], length * sizeof(T)); \
  }

DEFINE_MMEMCPY(int)
DEFINE_MMEMCPY(float)
DEFINE_MMEMCPY(double)

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

// matrix*_contiguous

__GHOST(matrix2_contiguous) {
  __requires("T: Type, matrix: ptr(T), a: int, b: int, n2: int, n1: int, M: int * int -> T");
  __consumes("for i in a..b -> for j in 0..n1 -> "
               "&matrix[MINDEX2(n2, n1, i, j)] ~~> M(i, j)");
  __produces("for k in a*n1..b*n1 -> &matrix[MINDEX1(n2*n1, k)] ~~> M(k/n1, k%n1)");
  __admitted();
}

__GHOST(matrix3_contiguous) {
  __requires("T: Type, matrix: ptr(T), a: int, b: int, n3: int, n2: int, n1: int, M: int * int * int -> T");
  __consumes("for i3 in a..b -> for i2 in 0..n2 -> "
             "for i1 in 0..n1 -> &matrix[MINDEX3(n3, n2, n1, i1, i2, i3)] ~~> M(i3, i2, i1)");
  __produces("for k in a*n2*n1..b*n2*n1 -> &matrix[MINDEX1(n3*n2*n1, k)] ~~> M(k/n1/n2, (k/n1) % n2, k%n1)");
  __admitted();
}

__GHOST(mindex2_contiguous) {
  __requires("T: Type, matrix: ptr(T), n2: int, i2: int, n1: int, a: int, b: int, M: int * int -> T");
  __consumes("for i1 in a..b -> &matrix[MINDEX2(n2, n1, i2, i1)] ~~> M(i2, i1)");
  __produces("for k in i2*n1 + a..i2*n1 + b -> &matrix[MINDEX1(n2*n1, k)] ~~> M(i2, k%n1)");
  __admitted();
}

__GHOST(mindex2_contiguous_rev) {
  __reverts(mindex2_contiguous);
  __admitted();
}

__GHOST(mindex3_contiguous) {
  __requires("T: Type, matrix: ptr(T), n3: int, i3: int, n2: int, i2: int, n1: int, a: int, b: int, M: int * int * int -> T");
  __consumes("for i1 in a..b -> &matrix[MINDEX3(n3, n2, n1, i3, i2, i1)] ~~> M(i3, i2, i1)");
  __produces("for k in (i3*n2*n1 + i2*n1 + a)..(i3*n2*n1 + i2*n1 + b) -> "
               "&matrix[MINDEX1(n3*n2*n1, k)] ~~> M(i3, i2, k%n1)");
  __admitted();
}

__GHOST(mindex3_contiguous_rev) {
  __reverts(mindex3_contiguous);
  __admitted();
}

__GHOST(mindex2_contiguous_uninit) {
  __requires("T: Type, matrix: ptr(T), n2: int, i2: int, n1: int, a: int, b: int");
  __consumes("for i1 in a..b -> &matrix[MINDEX2(n2, n1, i2, i1)] ~> UninitCell");
  __produces("for k in (i2*n1 + a)..(i2*n1 + b) -> "
               "&matrix[MINDEX1(n2*n1, k)] ~> UninitCell");
  __admitted();
}

__GHOST(mindex2_contiguous_uninit_rev) {
  __reverts(mindex2_contiguous_uninit);
  __admitted();
}

__GHOST(mindex3_contiguous_uninit) {
  __requires("T: Type, matrix: ptr(T), n3: int, i3: int, n2: int, i2: int, n1: int, a: int, b: int");
  __consumes("for i1 in a..b -> "
                "&matrix[MINDEX3(n3, n2, n1, i3, i2, i1)] ~> UninitCell");
  __produces("for k in (i3*n2*n1 + i2*n1 + a)..(i3*n2*n1 + i2*n1 + b) -> "
               "&matrix[MINDEX1(n3*n2*n1, k)] ~> UninitCell");
  __admitted();
}

__GHOST(mindex3_contiguous_uninit_rev) {
  __reverts(mindex3_contiguous_uninit);
  __admitted();
}

__GHOST(mindex2_contiguous_ro) {
  __requires("T: Type, matrix: ptr(T), n2: int, i2: int, n1: int, a: int, b: int, M: int * int -> T, f: _Fraction");
  __consumes("_RO(f, for i1 in a..b -> &matrix[MINDEX2(n2, n1, i2, i1)] ~~> M(i2, i1))");
  __produces("_RO(f, for k in (i2*n1 + a)..(i2*n1 + b) -> "
               "&matrix[MINDEX1(n2*n1, k)] ~~> M(i2, k%n1))");
  __admitted();
}

__GHOST(mindex2_contiguous_ro_rev) {
  __reverts(mindex2_contiguous_ro);
  __admitted();
}

__GHOST(mindex3_contiguous_ro) {
  __requires("T: Type, matrix: ptr(T), n3: int, i3: int, n2: int, i2: int, n1: int, a: int, b: int, M: int * int * int -> T, f: _Fraction");
  __consumes("_RO(f, for i1 in a..b -> "
                "&matrix[MINDEX3(n3, n2, n1, i3, i2, i1)] ~~> M(i3, i2, i1))");
  __produces("_RO(f, for k in (i3*n2*n1 + i2*n1 + a)..(i3*n2*n1 + i2*n1 + b) -> "
                "&matrix[MINDEX1(n3*n2*n1, k)] ~~> M(i3, i2, k%n1))");
  __admitted();
}

__GHOST(mindex3_contiguous_ro_rev) {
  __reverts(mindex3_contiguous_ro);
  __admitted();
}

/* ---- Algorithmic Functions ---- */

/* TODO: generalize to any monoid/group, see doc/sliding-window.md */
// template<typename T, typename ST>
/*inline uint16_t reduce_spe1(int start, int stop, uint8_t* input, int n, int m, int j) {
  __requires("check_range: is_subrange(start..stop, 0..n)");
  __requires("bound_check: in_range(j, 0..m)");
  __reads("input ~> Matrix2(n, m)");
  __admitted(); // NOT NECESSARY, BUT FASTER
  // __reads("for k in 0..n -> &input[MINDEX2(n, m, k, j)] ~> Cell");

  uint16_t s = (uint16_t)0;
  for (int i = start; i < stop; i++) {
    __smodifies("&s ~> Cell");
    __sreads("input ~> Matrix2(n, m)");

    __ghost(in_range_extend, "i, start..stop, 0..n");
    __GHOST_BEGIN(focus, ro_matrix2_focus, "input, i, j");
    s += (uint16_t)input[MINDEX2(n, m, i, j)];
    __GHOST_END(focus);
  }
  return s;
}*/

#endif
