#include <optitrust_common.h>

extern __ghost_unit __OPTITRUST_ENABLE_MODELS;

void MMEMCPY_int(int* dest, int d_offset, int* src, int s_offset, int length) {
  __requires("d_end: int");
  __requires("s_end: int");
  __requires("d_all: int");
  __requires("s_all: int");
  __requires("check_d_size: __is_true(d_end == d_offset + length)");
  __requires("check_s_size: __is_true(s_end == s_offset + length)");
  __requires("model: int -> int");
  __writes(
      "for k in d_offset..d_end -> &dest[MINDEX1(d_all, k)] ~~> model(k - "
      "d_offset + s_offset)");
  __reads("for k in s_offset..s_end -> &src[MINDEX1(s_all, k)] ~~> model(k)");
  __admitted();
  memcpy(&dest[d_offset], &src[s_offset], length * sizeof(int));
}

void MMEMCPY_float(float* dest, int d_offset, float* src, int s_offset,
                   int length) {
  __requires("d_end: int");
  __requires("s_end: int");
  __requires("d_all: int");
  __requires("s_all: int");
  __requires("check_d_size: __is_true(d_end == d_offset + length)");
  __requires("check_s_size: __is_true(s_end == s_offset + length)");
  __requires("model: int -> float");
  __writes(
      "for k in d_offset..d_end -> &dest[MINDEX1(d_all, k)] ~~> model(k - "
      "d_offset + s_offset)");
  __reads("for k in s_offset..s_end -> &src[MINDEX1(s_all, k)] ~~> model(k)");
  __admitted();
  memcpy(&dest[d_offset], &src[s_offset], length * sizeof(float));
}

void MMEMCPY_double(double* dest, int d_offset, double* src, int s_offset,
                    int length) {
  __requires("d_end: int");
  __requires("s_end: int");
  __requires("d_all: int");
  __requires("s_all: int");
  __requires("check_d_size: __is_true(d_end == d_offset + length)");
  __requires("check_s_size: __is_true(s_end == s_offset + length)");
  __requires("model: int -> double");
  __writes(
      "for k in d_offset..d_end -> &dest[MINDEX1(d_all, k)] ~~> model(k - "
      "d_offset + s_offset)");
  __reads("for k in s_offset..s_end -> &src[MINDEX1(s_all, k)] ~~> model(k)");
  __admitted();
  memcpy(&dest[d_offset], &src[s_offset], length * sizeof(double));
}

__ghost_ret matrix1_ro_focus() {
  __requires("T: Type");
  __requires("p: ptr(T)");
  __requires("i: int");
  __requires("n: int");
  __requires("M: int -> T");
  __requires("f: _Fraction");
  __requires("bound_check: in_range(i, 0..n)");
  __consumes("_RO(f, p ~> Matrix1(n, M))");
  __produces(
      "Wand(_RO(f, &p[MINDEX1(n, i)] ~~> M(i)), _RO(f, p ~> Matrix1(n, M)))");
  __produces("_RO(f, &p[MINDEX1(n, i)] ~~> M(i))");
  __admitted();
  __ghost(group_ro_focus, "f := f, i := i, bound_check := bound_check");
}

__ghost_ret matrix1_ro_unfocus() {
  __reverts(matrix1_ro_focus);
  __admitted();
  __ghost(close_wand);
}

__ghost_ret matrix2_ro_focus() {
  __requires("T: Type");
  __requires("p: ptr(T)");
  __requires("i: int");
  __requires("j: int");
  __requires("m: int");
  __requires("n: int");
  __requires("M: int * int -> T");
  __requires("f: _Fraction");
  __requires("bound_check_i: in_range(i, 0..m)");
  __requires("bound_check_j: in_range(j, 0..n)");
  __consumes("_RO(f, p ~> Matrix2(m, n, M))");
  __produces(
      "Wand(_RO(f, &p[MINDEX2(m, n, i, j)] ~~> M(i, j)), _RO(f, p ~> "
      "Matrix2(m, n, M)))");
  __produces("_RO(f, &p[MINDEX2(m, n, i, j)] ~~> M(i, j))");
  __admitted();
  __ghost(group_ro_focus, "f := f, i := i, bound_check := bound_check_i");
  __ghost(group_ro_focus, "f := f, i := j, bound_check := bound_check_j");
  __ghost(wand_simplify);
}

__ghost_ret matrix2_ro_unfocus() {
  __reverts(matrix2_ro_focus);
  __admitted();
  __ghost(close_wand);
}

__ghost_ret matrix2_contiguous() {
  __requires("T: Type");
  __requires("p: ptr(T)");
  __requires("a: int");
  __requires("b: int");
  __requires("n2: int");
  __requires("n1: int");
  __requires("M: int * int -> T");
  __consumes(
      "for i in a..b -> for j in 0..n1 -> &p[MINDEX2(n2, n1, i, j)] ~~> M(i, "
      "j)");
  __produces(
      "for k in (a * n1)..(b * n1) -> &p[MINDEX1(n2 * n1, k)] ~~> M(k / n1, k "
      "% n1)");
  __admitted();
}

__ghost_ret matrix3_contiguous() {
  __requires("T: Type");
  __requires("p: ptr(T)");
  __requires("a: int");
  __requires("b: int");
  __requires("n3: int");
  __requires("n2: int");
  __requires("n1: int");
  __requires("M: int * int * int -> T");
  __consumes(
      "for i3 in a..b -> for i2 in 0..n2 -> for i1 in 0..n1 -> &p[MINDEX3(n3, "
      "n2, n1, i1, i2, i3)] ~~> M(i3, i2, i1)");
  __produces(
      "for k in (a * n2 * n1)..(b * n2 * n1) -> &p[MINDEX1(n3 * n2 * n1, k)] "
      "~~> M(k / n1 / n2, k / n1 % n2, k % n1)");
  __admitted();
}

__ghost_ret mindex2_contiguous() {
  __requires("T: Type");
  __requires("p: ptr(T)");
  __requires("n2: int");
  __requires("i2: int");
  __requires("n1: int");
  __requires("a: int");
  __requires("b: int");
  __requires("M: int * int -> T");
  __consumes("for i1 in a..b -> &p[MINDEX2(n2, n1, i2, i1)] ~~> M(i2, i1)");
  __produces(
      "for k in (i2 * n1 + a)..(i2 * n1 + b) -> &p[MINDEX1(n2 * n1, k)] ~~> "
      "M(i2, k % n1)");
  __admitted();
}

__ghost_ret mindex2_contiguous_rev() {
  __reverts(mindex2_contiguous);
  __admitted();
}

__ghost_ret mindex3_contiguous() {
  __requires("T: Type");
  __requires("p: ptr(T)");
  __requires("n3: int");
  __requires("i3: int");
  __requires("n2: int");
  __requires("i2: int");
  __requires("n1: int");
  __requires("a: int");
  __requires("b: int");
  __requires("M: int * int * int -> T");
  __consumes(
      "for i1 in a..b -> &p[MINDEX3(n3, n2, n1, i3, i2, i1)] ~~> M(i3, i2, "
      "i1)");
  __produces(
      "for k in (i3 * n2 * n1 + i2 * n1 + a)..(i3 * n2 * n1 + i2 * n1 + b) -> "
      "&p[MINDEX1(n3 * n2 * n1, k)] ~~> M(i3, i2, k % n1)");
  __admitted();
}

__ghost_ret mindex3_contiguous_rev() {
  __reverts(mindex3_contiguous);
  __admitted();
}

__ghost_ret mindex2_contiguous_uninit() {
  __requires("T: Type");
  __requires("p: ptr(T)");
  __requires("n2: int");
  __requires("i2: int");
  __requires("n1: int");
  __requires("a: int");
  __requires("b: int");
  __consumes("for i1 in a..b -> &p[MINDEX2(n2, n1, i2, i1)] ~> UninitCell");
  __produces(
      "for k in (i2 * n1 + a)..(i2 * n1 + b) -> &p[MINDEX1(n2 * n1, k)] ~> "
      "UninitCell");
  __admitted();
}

__ghost_ret mindex2_contiguous_uninit_rev() {
  __reverts(mindex2_contiguous_uninit);
  __admitted();
}

__ghost_ret mindex3_contiguous_uninit() {
  __requires("T: Type");
  __requires("p: ptr(T)");
  __requires("n3: int");
  __requires("i3: int");
  __requires("n2: int");
  __requires("i2: int");
  __requires("n1: int");
  __requires("a: int");
  __requires("b: int");
  __consumes(
      "for i1 in a..b -> &p[MINDEX3(n3, n2, n1, i3, i2, i1)] ~> UninitCell");
  __produces(
      "for k in (i3 * n2 * n1 + i2 * n1 + a)..(i3 * n2 * n1 + i2 * n1 + b) -> "
      "&p[MINDEX1(n3 * n2 * n1, k)] ~> UninitCell");
  __admitted();
}

__ghost_ret mindex3_contiguous_uninit_rev() {
  __reverts(mindex3_contiguous_uninit);
  __admitted();
}

__ghost_ret mindex2_contiguous_ro() {
  __requires("T: Type");
  __requires("p: ptr(T)");
  __requires("n2: int");
  __requires("i2: int");
  __requires("n1: int");
  __requires("a: int");
  __requires("b: int");
  __requires("M: int * int -> T");
  __requires("f: _Fraction");
  __consumes(
      "_RO(f, for i1 in a..b -> &p[MINDEX2(n2, n1, i2, i1)] ~~> M(i2, i1))");
  __produces(
      "_RO(f, for k in (i2 * n1 + a)..(i2 * n1 + b) -> &p[MINDEX1(n2 * n1, k)] "
      "~~> M(i2, k % n1))");
  __admitted();
}

__ghost_ret mindex2_contiguous_ro_rev() {
  __reverts(mindex2_contiguous_ro);
  __admitted();
}

__ghost_ret mindex3_contiguous_ro() {
  __requires("T: Type");
  __requires("p: ptr(T)");
  __requires("n3: int");
  __requires("i3: int");
  __requires("n2: int");
  __requires("i2: int");
  __requires("n1: int");
  __requires("a: int");
  __requires("b: int");
  __requires("M: int * int * int -> T");
  __requires("f: _Fraction");
  __consumes(
      "_RO(f, for i1 in a..b -> &p[MINDEX3(n3, n2, n1, i3, i2, i1)] ~~> M(i3, "
      "i2, i1))");
  __produces(
      "_RO(f, for k in (i3 * n2 * n1 + i2 * n1 + a)..(i3 * n2 * n1 + i2 * n1 + "
      "b) -> &p[MINDEX1(n3 * n2 * n1, k)] ~~> M(i3, i2, k % n1))");
  __admitted();
}

__ghost_ret mindex3_contiguous_ro_rev() {
  __reverts(mindex3_contiguous_ro);
  __admitted();
}
