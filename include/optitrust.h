#ifndef OPTITRUST_H
#define OPTITRUST_H

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <assert.h>
#include <string.h>
#include <functional>

/* ---- Resource Annotations ---- */

inline void __pure() {}
inline void __requires(const char*) {}
inline void __ensures(const char*) {}
inline void __invariant(const char*) {}
inline void __reads(const char*) {}
inline void __writes(const char*) {}
inline void __modifies(const char*) {}
inline void __consumes(const char*) {}
inline void __produces(const char*) {}
inline void __sequentially_reads(const char*) {}
inline void __sequentially_modifies(const char*) {}

inline void __admitted() {}

/* ---- Ghost annotations ---- */

// Return type for ghost functions
typedef void __ghost_ret;

// Type of ghost function pointers
typedef std::function<__ghost_ret()> __ghost_fn;

// Argument type for ghost functions
typedef const char* __ghost_args;

// Marcro for ghost function prototype
#define __GHOST(f) inline __ghost_ret f()

// Invoke a ghost function
inline void __ghost(__ghost_fn, __ghost_args) {}

/// Postfix call for specifying ghost arguments
inline void __with(__ghost_args) {}
template<typename T> T __call_with(T ret_val, __ghost_args) { return ret_val; }

// TODO: bind et call_with
/*
template<typename T> T __bind(T ret_val, const char*) { return ret_val; }
inline void __rename(const char*) {}
*/

inline __ghost_fn __ghost_begin(__ghost_fn, __ghost_args) { return __admitted; }
inline void __ghost_end(__ghost_fn) {}

#define __GHOST_BEGIN(rev_ghost, ghost, ghost_args) const __ghost_fn rev_ghost = __ghost_begin(ghost, ghost_args)
#define __GHOST_END(rev_ghost) __ghost_end(rev_ghost)

inline __ghost_fn __with_reverse(__ghost_fn g, __ghost_fn g_rev) { return g; }
inline void __reverts(__ghost_fn) {}

#define __GHOST_BEGIN_CUSTOM(rev_ghost, forward_ghost, backward_ghost) __GHOST_BEGIN(rev_ghost, __with_reverse(forward_ghost, backward_ghost), "")

/* ---- Contract for primitive functions ---- */

template<typename T> T* __new(T init) {
  __produces("_Res ~> Cell");
  __admitted();
  return new T(init);
}

template<typename T> T __get(T* p) {
  __reads("p ~> Cell");
  __admitted();
  return *p;
}

template<typename T> void __set(T* p, T x) {
  __writes("p ~> Cell");
  __admitted();
  *p = x;
}

template<typename T> T __add(T x1, T x2) {
  __pure();
  __admitted();
  return x1 + x2;
}

template<typename T> T __sub(T x1, T x2) {
  __pure();
  __admitted();
  return x1 - x2;
}

template<typename T> T __mul(T x1, T x2) {
  __pure();
  __admitted();
  return x1 * x2;
}

template<typename T> T* __array_access(T* tab, int i) {
  __pure();
  __admitted();
  return &tab[i];
}

template<typename T> void __add_inplace(T* p, T x) {
  __modifies("p ~> Cell");
  __admitted();
  *p += x;
}

template<typename T> void __sub_inplace(T* p, T x) {
  __modifies("p ~> Cell");
  __admitted();
  *p -= x;
}

template<typename T> void __mul_inplace(T* p, T x) {
  __modifies("p ~> Cell");
  __admitted();
  *p *= x;
}

template<typename T> T __post_inc(T* p) {
  __modifies("p ~> Cell");
  __admitted();
  return p++;
}

template<typename T> T __post_dec(T* p) {
  __modifies("p ~> Cell");
  __admitted();
  return p--;
}

template<typename T> T __pre_inc(T* p) {
  __modifies("p ~> Cell");
  __admitted();
  return ++p;
}

template<typename T> T __pre_dec(T* p) {
  __modifies("p ~> Cell");
  __admitted();
  return --p;
}

/* ---- Matrix Functions ---- */

inline int MINDEX0() {
  __pure();
  __admitted();
  return 0;
}

inline int MINDEX1(int N1, int i1) {
  __pure();
  __admitted();
  return i1;
}

inline int MINDEX2(int N1, int N2, int i1, int i2) {
  __pure();
  __admitted();
  return i1 * N2 + i2;
}

inline int MINDEX3(int N1, int N2, int N3, int i1, int i2, int i3) {
  __pure();
  __admitted();
  return i1 * N2 * N3 + i2 * N3 + i3;
}

inline int MINDEX4(int N1, int N2, int N3, int N4, int i1, int i2, int i3, int i4) {
  __pure();
  __admitted();
  return i1 * N2 * N3 * N4 + i2 * N3 * N4 + i3 * N4 + i4;
}

inline void* CALLOC1(int N1, size_t bytes_per_item) {
  __produces("_Res ~> Matrix1(N1)");
  __admitted();
  return calloc(N1, bytes_per_item);
}

inline void* CALLOC2(int N1, int N2, size_t bytes_per_item) {
  __produces("_Res ~> Matrix2(N1, N2)");
  __admitted();
  return calloc(N1 * N2, bytes_per_item);
}

inline void* CALLOC3(int N1, int N2, int N3, size_t bytes_per_item) {
  __produces("_Res ~> Matrix3(N1, N2, N3)");
  __admitted();
  return calloc(N1 * N2 * N3, bytes_per_item);
}

inline void* CALLOC4(int N1, int N2, int N3, int N4, size_t bytes_per_item) {
  __produces("_Res ~> Matrix4(N1, N2, N3, N4)");
  __admitted();
  return calloc(N1 * N2 * N3 * N4, bytes_per_item);
}

inline void* MALLOC0(size_t bytes_per_item) {
  __produces("_Uninit(_Res ~> Matrix0())");
  __admitted();
  return malloc(bytes_per_item);
}

inline void* MALLOC1(int N1, size_t bytes_per_item) {
  __produces("_Uninit(_Res ~> Matrix1(N1))");
  __admitted();
  return malloc(N1 * bytes_per_item);
}

inline void* MALLOC2(int N1, int N2, size_t bytes_per_item) {
  __produces("_Uninit(_Res ~> Matrix2(N1, N2))");
  __admitted();
  return malloc(N1 * N2 * bytes_per_item);
}

inline void* MALLOC3(int N1, int N2, int N3, size_t bytes_per_item) {
  __produces("_Uninit(_Res ~> Matrix3(N1, N2, N3))");
  __admitted();
  return malloc(N1 * N2 * N3 * bytes_per_item);
}

inline void* MALLOC4(int N1, int N2, int N3, int N4, size_t bytes_per_item) {
  __produces("_Uninit(_Res ~> Matrix4(N1, N2, N3, N4))");
  __admitted();
  return malloc(N1 * N2 * N3 * N4 * bytes_per_item);
}

inline void MFREE0(void* p) {
  __consumes("_Uninit(p ~> Matrix0())");
  __admitted();
  free(p);
}

inline void MFREE1(int N1, void* p) {
  __consumes("_Uninit(p ~> Matrix1(N1))");
  __admitted();
  free(p);
}

inline void MFREE2(int N1, int N2, void* p) {
  __consumes("_Uninit(p ~> Matrix2(N1, N2))");
  __admitted();
  free(p);
}

inline void MFREE3(int N1, int N2, int N3, void* p) {
  __consumes("_Uninit(p ~> Matrix3(N1, N2, N3))");
  __admitted();
  free(p);
}

inline void MFREE4(int N1, int N2, int N3, int N4, void* p) {
  __consumes("_Uninit(p ~> Matrix4(N1, N2, N3, N4))");
  __admitted();
  free(p);
}

inline void MMEMCPY(void*__restrict__ dest, size_t d_offset,
                    const void*__restrict__ src, size_t s_offset,
                    size_t elems, size_t bytes_per_item) {
  __requires("d_end: size_t, s_end: size_t, d_all: size_t, s_all: size_t");
  // __requires("check_range: 0 <= d_offset <= d_end && 0 <= s_offset <= s_end");
  // __requires("check_dim: d_end <= d_all && s_end <= s_all");
  // __requires("check_elems: elems == d_end - d_offset == s_end - s_offset");
  __writes("Group(range(d_offset, d_end, 1), fun k -> &dest[MINDEX1(d_all, k)] ~> Cell)");
  __reads("Group(range(s_offset, s_end, 1), fun k -> &src[MINDEX1(s_all, k)] ~> Cell)");
  __admitted();

  size_t dest2 = (size_t)(dest) + d_offset*bytes_per_item;
  size_t src2 = (size_t)(src) + s_offset*bytes_per_item;
  memcpy((void*)(dest2), (void*)(src2), elems*bytes_per_item);
}

/* ---- Ghosts ---- */

__GHOST(rewrite) {
  __requires("H1: formula, H2: formula, by: H1 = H2");
  __consumes("H1");
  __produces("H2");
  __admitted();
}

__GHOST(close_wand) {
  /* LATER: Replace that id with a generated name on the respective open */
  __requires("H1: formula, H2: formula");
  __consumes("Wand(H1, H2), H1");
  __produces("H2");
  __admitted();
}

__GHOST(wand_simplify) {
  __requires("H1: formula, H2: formula, H3: formula");
  __consumes("Wand(H1, H2), Wand(H2, H3)");
  __produces("Wand(H1, H3)");
  __admitted();
}

// NOTE: calling forget_init on _RO or _Uninit is possible but useless
__GHOST(forget_init) {
  __requires("H: formula");
  __consumes("H");
  __produces("_Uninit(H)");
  __admitted();
}

/* ---- Group Ghosts ---- */

__GHOST(ro_fork_group) {
  __requires("f: _Fraction, H: formula, r: range");
  __consumes("_RO(f, H)");
  // TODO: can we write: Group(r, fun _ -> _RO(f / range_count(r), H)) ?
  __produces("_RO(f / range_count(r), Group(r, fun _ -> H))");
  __admitted();
}

__GHOST(ro_join_group) {
  __reverts(ro_fork_group);
  __admitted();
}

/* identities due to normal form:

__GHOST(ro_distribute_group) {
  __requires("range: range, items: int -> formula, f: _Fraction");
  __consumes("_RO(f, Group(range, items))");
  __produces("Group(range, fun i -> _RO(f, items(i)))");
  __admitted();
}

__GHOST(ro_factorize_group) {
  __reverts(ro_distribute_group)
  __admitted();
}
*/

__GHOST(tile_divides) {
  __requires(
    "tile_count: int, tile_size: int,"
    "n: int, to_item: int -> resource,"
    "bound_check: n = tile_size * tile_count"
  );
  __consumes("Group(range(0, n, 1), to_item)");
  __produces("Group(range(0, tile_count, 1), fun bi ->"
               "Group(range(0, tile_size, 1), fun i -> to_item(bi * tile_size + i)))");
  __admitted();
}

__GHOST(untile_divides) {
  __reverts(tile_divides);
  __admitted();
}

__GHOST(ro_tile_divides) {
  __requires(
    "tile_count: int, tile_size: int,"
    "n: int, to_item: int -> resource,"
    "bound_check: n = tile_size * tile_count,"
    "f: _Fraction"
  );
  __consumes("_RO(f, Group(range(0, n, 1), to_item))");
  __produces("_RO(f, Group(range(0, tile_count, 1), fun bi ->"
               "Group(range(0, tile_size, 1), fun i -> to_item(bi * tile_size + i))))");
  __admitted();
}

__GHOST(ro_untile_divides) {
  __requires(
    "tile_count: int, tile_size: int,"
    "n: int, to_item: int -> resource,"
    "bound_check: n = tile_size * tile_count,"
    "f: _Fraction"
  );
  __consumes("_RO(_Full(f), Group(range(0, tile_count, 1), fun bi ->"
               "Group(range(0, tile_size, 1), fun i -> to_item(bi * tile_size + i))))");
  __produces("_RO(f, Group(range(0, n, 1), to_item))");
  __admitted();
}

__GHOST(group_focus) {
  __requires("i: int, start: int, stop: int, step: int, items: int -> formula");
  __requires("bound_check_start: start <= i, bound_check_stop: i < stop, bound_check_step: (start + i) % step = 0");
  __consumes("Group(range(start, stop, step), items)");
  __produces("Wand(items(i), Group(range(start, stop, step), items)), items(i)");
  __admitted();
}

__GHOST(group_unfocus) {
  __reverts(group_focus);

  __ghost(close_wand, "");
}

__GHOST(group_ro_focus) {
  __requires("i: int, start: int, stop: int, step: int, items: int -> formula, f: _Fraction");
  __requires("bound_check_start: start <= i, bound_check_stop: i < stop, bound_check_step: (start + i) % step = 0");
  __consumes("_RO(f, Group(range(start, stop, step), items))");
  __produces("Wand(_RO(f, items(i)), _RO(f, Group(range(start, stop, step), items))), _RO(f, items(i))");
  __admitted();
}

__GHOST(group_ro_unfocus) {
  __reverts(group_ro_focus);

  __ghost(close_wand, "");
}

__GHOST(group_focus_subrange) {
  __requires("start: int, stop: int, step: int, old_start: int, old_stop: int, items: int -> formula");
  // __requires("bound_check_start: old_start <= start, bound_check_stop: stop <= old_stop");
  // TODO: old_start <= start && stop <= old_stop && step > 0
  __consumes("Group(range(old_start, old_stop, step), items)");
  __produces("Wand(Group(range(start, stop, step), items), Group(range(old_start, old_stop, step), items)), Group(range(start, stop, step), items)");
  __admitted();
}

__GHOST(group_unfocus_subrange) {
  __reverts(group_focus_subrange);
  __ghost(close_wand, "");
}

__GHOST(group_focus_subrange_uninit) {
  __requires("start: int, stop: int, step: int, old_start: int, old_stop: int, items: int -> formula");
  // __requires("bound_check_start: old_start <= start, bound_check_stop: stop <= old_stop");
  // TODO: old_start <= start && stop <= old_stop && step > 0
  __consumes("_Uninit(Group(range(old_start, old_stop, step), items))");
  __produces("Wand("
    "_Uninit(Group(range(start, stop, step), items)),"
    "_Uninit(Group(range(old_start, old_stop, step), items))"
    "),"
    "_Uninit(Group(range(start, stop, step), items))");
  __admitted();
}

__GHOST(group_unfocus_subrange_uninit) {
  __reverts(group_focus_subrange_uninit);
  __ghost(close_wand, "");
}

__GHOST(group2_focus_subrange_uninit) {
  __requires("start: int, stop: int, step: int, old_start: int, old_stop: int, items: int -> int -> formula,"
    " ra1: int, rb1: int, rs1: int");
  // __requires("bound_check_start: old_start <= start, bound_check_stop: stop <= old_stop");
  // TODO: old_start <= start && stop <= old_stop && step > 0
  __consumes("_Uninit(Group(range(ra1, rb1, rs1), fun i -> Group(range(old_start, old_stop, step), items(i))))");
  __produces("Wand("
    "_Uninit(Group(range(ra1, rb1, rs1), fun i -> Group(range(start, stop, step), items(i)))),"
    "_Uninit(Group(range(ra1, rb1, rs1), fun i -> Group(range(old_start, old_stop, step), items(i))))"
    "),"
    "_Uninit(Group(range(ra1, rb1, rs1), fun i -> Group(range(start, stop, step), items(i))))");

  __admitted();
  /* for (int i = ra1; i < rb1; i += rs1) {
    __ghost(group_focus_subrange_uninit, ...);
  } */
}

__GHOST(group2_unfocus_subrange_uninit) {
  __reverts(group2_focus_subrange_uninit);
  __admitted();
  /* for (int i = ra1; i < rb1; i += rs1) {
    __ghost(group_unfocus_subrange_uninit, ...);
  } */
}

/* --- group_shift and nested variants: */

__GHOST(group_shift) {
  __requires("a: int, b: int, step: int, items: int -> formula");
  __requires("a2: int, b2: int");
  // __requires("bound_check: (b2 - b) == (a2 - b) || (b - a) == (b2 - a2)");
  __consumes("Group(range(a, b, step), fun i -> items(i))");
  __produces("Group(range(a2, b2, step), fun i -> items(i - a2 + a))");
  __admitted();
}

__GHOST(group_unshift) {
  __reverts(group_shift);
  __admitted();
}

__GHOST(group2_shift) {
  __requires("a: int, b: int, step: int, items: int -> int -> formula");
  __requires("a2: int, b2: int");
  __requires("range1: formula");
  // __requires bound_check ...
  __consumes("Group(range1, fun i1 -> Group(range(a, b, step), fun i -> items(i1)(i)))");
  __produces("Group(range1, fun i1 -> Group(range(a2, b2, step),"
               " fun i -> items(i1)(i - a2 + a)))");
  __admitted();
}

__GHOST(group2_unshift) {
  __reverts(group2_shift);
  __admitted();
}

__GHOST(group3_shift) {
  __requires("a: int, b: int, step: int, items: int -> int -> int -> formula");
  __requires("a2: int, b2: int");
  __requires("range1: formula, range2: formula");
  // __requires bound_check ...
  __consumes("Group(range1, fun i1 -> Group(range2, fun i2 -> "
               "Group(range(a, b, step), fun i -> items(i1)(i2)(i))))");
  __produces("Group(range1, fun i1 -> Group(range2, fun i2 -> "
               "Group(range(a2, b2, step), fun i -> items(i1)(i2)(i - a2 + a))))");
  __admitted();
}

__GHOST(group3_unshift) {
  __reverts(group3_shift);
  __admitted();
}

/* --- group_shift_uninit and nested variants: */

__GHOST(group_shift_uninit) {
  __requires("a: int, b: int, step: int, items: int -> formula");
  __requires("a2: int, b2: int");
  // __requires bound_check ...
  __consumes("_Uninit(Group(range(a, b, step), fun i -> items(i)))");
  __produces("_Uninit(Group(range(a2, b2, step), fun i -> items(i - a2 + a)))");
  __admitted();
}

__GHOST(group_unshift_uninit) {
  __reverts(group_shift_uninit);
  __admitted();
}

__GHOST(group2_shift_uninit) {
  __requires("a: int, b: int, step: int, items: int -> int -> formula");
  __requires("a2: int, b2: int");
  __requires("range1: formula");
  // __requires bound_check ...
  __consumes("_Uninit(Group(range1, fun i1 -> "
               "Group(range(a, b, step), fun i -> items(i1)(i))))");
  __produces("_Uninit(Group(range1, fun i1 -> "
               "Group(range(a2, b2, step), "
                 "fun i -> items(i1)(i - a2 + a))))");
  __admitted();
}

__GHOST(group2_unshift_uninit) {
  __reverts(group2_shift_uninit);
  __admitted();
}

__GHOST(group3_shift_uninit) {
  __requires("a: int, b: int, step: int, items: int -> int -> int -> formula");
  __requires("a2: int, b2: int");
  __requires("range1: formula, range2: formula");
  // __requires bound_check ...
  __consumes("_Uninit(Group(range1, fun i1 -> Group(range2, fun i2 -> "
               "Group(range(a, b, step), fun i -> items(i1)(i2)(i)))))");
  __produces("_Uninit(Group(range1, fun i1 -> Group(range2, fun i2 -> "
               "Group(range(a2, b2, step), fun i -> items(i1)(i2)(i - a2 + a)))))");
  __admitted();
}

__GHOST(group3_unshift_uninit) {
  __reverts(group3_shift_uninit);
  __admitted();
}

/* --- group_shift_ro and nested variants: */

__GHOST(group_shift_ro) {
  __requires("a: int, b: int, step: int, items: int -> formula");
  __requires("a2: int, b2: int");
  __requires("f: _Fraction");
  // __requires bound_check ...
  __consumes("_RO(f, Group(range(a, b, step), fun i -> items(i)))");
  __produces("_RO(f, Group(range(a2, b2, step), fun i -> items(i - a2 + a)))");
  __admitted();
}

__GHOST(group_unshift_ro) {
  __reverts(group_shift_ro);
  __admitted();
}

__GHOST(group2_shift_ro) {
  __requires("a: int, b: int, step: int, items: int -> int -> formula");
  __requires("a2: int, b2: int");
  __requires("range1: formula");
  __requires("f: _Fraction");
  // __requires bound_check ...
  __consumes("_RO(f, Group(range1, fun i1 -> "
               "Group(range(a, b, step), fun i -> items(i1)(i))))");
  __produces("_RO(f, Group(range1, fun i1 -> "
               "Group(range(a2, b2, step), "
                 "fun i -> items(i1)(i - a2 + a))))");
  __admitted();
}

__GHOST(group2_unshift_ro) {
  __reverts(group2_shift_ro);
  __admitted();
}

__GHOST(group3_shift_ro) {
  __requires("a: int, b: int, step: int, items: int -> int -> int -> formula");
  __requires("a2: int, b2: int");
  __requires("range1: formula, range2: formula");
  __requires("f: _Fraction");
  // __requires bound_check ...
  __consumes("_RO(f, Group(range1, fun i1 -> Group(range2, fun i2 -> "
               "Group(range(a, b, step), fun i -> items(i1)(i2)(i)))))");
  __produces("_RO(f, Group(range1, fun i1 -> Group(range2, fun i2 -> "
               "Group(range(a2, b2, step), fun i -> items(i1)(i2)(i - a2 + a)))))");
  __admitted();
}

__GHOST(group3_unshift_ro) {
  __reverts(group3_shift_ro);
  __admitted();
}

/* ---- Matrix Ghosts ---- */

// FIXME: matrixN_*focus ghosts are not checking bounds
__GHOST(matrix2_focus)  {
  __requires("M: ptr, i: int, j: int, m: int, n: int");
  __consumes("M ~> Matrix2(m, n)");
  __produces("Wand(&M[MINDEX2(m, n, i, j)] ~> Cell, M ~> Matrix2(m, n)), &M[MINDEX2(m, n, i, j)] ~> Cell");

  __ghost(group_focus, "i := i, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
  __ghost(group_focus, "i := j, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
  __ghost(wand_simplify, "");
}

__GHOST(matrix2_unfocus) {
  __reverts(matrix2_focus);

  __ghost(close_wand, "");
}

__GHOST(matrix1_ro_focus) {
  __requires("M: ptr, i: int, n: int, f: _Fraction");
  __consumes("_RO(f, M ~> Matrix1(n))");
  __produces("Wand(_RO(f, &M[MINDEX1(n, i)] ~> Cell), _RO(f, M ~> Matrix1(n))), _RO(f, &M[MINDEX1(n, i)] ~> Cell)");

  __ghost(group_ro_focus, "f := f, i := i, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
}

__GHOST(matrix1_ro_unfocus) {
  __reverts(matrix1_ro_focus);

  __ghost(close_wand, "");
}

__GHOST(matrix2_ro_focus) {
  __requires("M: ptr, i: int, j: int, m: int, n: int, f: _Fraction");
  __consumes("_RO(f, M ~> Matrix2(m, n))");
  __produces("Wand(_RO(f, &M[MINDEX2(m, n, i, j)] ~> Cell), _RO(f, M ~> Matrix2(m,n))), _RO(f, &M[MINDEX2(m, n, i, j)] ~> Cell)");

  __ghost(group_ro_focus, "f := f, i := i, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
  __ghost(group_ro_focus, "f := f, i := j, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
  __ghost(wand_simplify, "");
}

__GHOST(matrix2_ro_unfocus) {
  __reverts(matrix2_ro_focus);

  __ghost(close_wand, "");
}

__GHOST(matrix2_contiguous) {
  __requires("M: ptr, a2: int, b2: int, n2: int, n1: int");
  // __requires("check_range: 0 <= a2 <= b2 <= n2 && 0 <= n1");
  __consumes("Group(range(a2, b2, 1), fun i -> Group(range(0, n1, 1), fun j -> "
               "&M[MINDEX2(n2, n1, i, j)] ~> Cell))");
  __produces("Group(range(a2*n1, b2*n1, 1), fun k -> &M[MINDEX1(n2*n1, k)] ~> Cell)");
  __admitted();
}

__GHOST(matrix3_contiguous) {
  __requires("M: ptr, a3: int, b3: int, n3: int, n2: int, n1: int");
  // __requires("check_range: 0 <= a3 <= b3 <= n3 && 0 <= n2 && 0 <= n1");
  __consumes("Group(range(a3, b3, 1), fun i3 -> Group(range(0, n2, 1), fun i2 -> "
             "Group(range(0, n1, 1), fun i1 -> &M[MINDEX3(n3, n2, n1, i1, i2, i3)] ~> Cell)))");
  __produces("Group(range(a3*n2*n1, b3*n2*n1, 1), fun k -> &M[MINDEX1(n3*n2*n1, k)] ~> Cell)");
  __admitted();
}

__GHOST(mindex2_contiguous) {
  __requires("M: ptr, n2: int, i2: int, n1: int, a: int, b: int");
  __consumes("Group(range(a, b, 1), fun i1 -> &M[MINDEX2(n2, n1, i2, i1)] ~> Cell)");
  __produces("Group(range(i2*n1 + a, i2*n1 + b, 1), fun k -> &M[MINDEX1(n2*n1, k)] ~> Cell)");
  __admitted();
}

__GHOST(mindex2_contiguous_rev) {
  __reverts(mindex2_contiguous);
  __admitted();
}

__GHOST(mindex3_contiguous) {
  __requires("M: ptr, n3: int, i3: int, n2: int, i2: int, n1: int, a: int, b: int");
  __consumes("Group(range(a, b, 1), fun i1 -> &M[MINDEX3(n3, n2, n1, i3, i2, i1)] ~> Cell)");
  __produces("Group(range(i3*n2*n1 + i2*n1 + a, i3*n2*n1 + i2*n1 + b, 1), "
               "fun k -> &M[MINDEX1(n3*n2*n1, k)] ~> Cell)");
  __admitted();
}

__GHOST(mindex3_contiguous_rev) {
  __reverts(mindex3_contiguous);
  __admitted();
}

__GHOST(mindex2_contiguous_uninit) {
  __requires("M: ptr, n2: int, i2: int, n1: int, a: int, b: int");
  __consumes("_Uninit(Group(range(a, b, 1), fun i1 -> &M[MINDEX2(n2, n1, i2, i1)] ~> Cell))");
  __produces("_Uninit(Group(range(i2*n1 + a, i2*n1 + b, 1), "
               "fun k -> &M[MINDEX1(n2*n1, k)] ~> Cell))");
  __admitted();
}

__GHOST(mindex2_contiguous_uninit_rev) {
  __reverts(mindex2_contiguous_uninit);
  __admitted();
}

__GHOST(mindex3_contiguous_uninit) {
  __requires("M: ptr, n3: int, i3: int, n2: int, i2: int, n1: int, a: int, b: int");
  __consumes("_Uninit(Group(range(a, b, 1), fun i1 -> "
                "&M[MINDEX3(n3, n2, n1, i3, i2, i1)] ~> Cell))");
  __produces("_Uninit(Group(range(i3*n2*n1 + i2*n1 + a, i3*n2*n1 + i2*n1 + b, 1), "
               "fun k -> &M[MINDEX1(n3*n2*n1, k)] ~> Cell))");
  __admitted();
}

__GHOST(mindex3_contiguous_uninit_rev) {
  __reverts(mindex3_contiguous_uninit);
  __admitted();
}

__GHOST(mindex2_contiguous_ro) {
  __requires("M: ptr, n2: int, i2: int, n1: int, a: int, b: int, f: _Fraction");
  __consumes("_RO(f, Group(range(a, b, 1), fun i1 -> &M[MINDEX2(n2, n1, i2, i1)] ~> Cell))");
  __produces("_RO(f, Group(range(i2*n1 + a, i2*n1 + b, 1), "
               "fun k -> &M[MINDEX1(n2*n1, k)] ~> Cell))");
  __admitted();
}

__GHOST(mindex2_contiguous_ro_rev) {
  __reverts(mindex2_contiguous_ro);
  __admitted();
}

__GHOST(mindex3_contiguous_ro) {
  __requires("M: ptr, n3: int, i3: int, n2: int, i2: int, n1: int, a: int, b: int, f: _Fraction");
  __consumes("_RO(f, Group(range(a, b, 1), fun i1 -> "
                "&M[MINDEX3(n3, n2, n1, i3, i2, i1)] ~> Cell))");
  __produces("_RO(f, Group(range(i3*n2*n1 + i2*n1 + a, i3*n2*n1 + i2*n1 + b, 1), "
               "fun k -> &M[MINDEX1(n3*n2*n1, k)] ~> Cell))");
  __admitted();
}

__GHOST(mindex3_contiguous_ro_rev) {
  __reverts(mindex3_contiguous_ro);
  __admitted();
}

/* ---- Arithmetic Functions ---- */

inline int exact_div(int n, int b) {
  __pure();
  __admitted();
  return n / b;
}

/* ---- Other Functions ---- */

inline void MFREE(void* p) {
  free(p);
}

inline int ANY(int maxValue) { return 0; }

#endif
