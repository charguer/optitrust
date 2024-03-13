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
inline void __parallel_reads(const char*) {}
inline void __loop_ghosts(const char*) {}

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
  __requires("check_d_size: d_end = d_offset + elems");
  __requires("check_s_size: s_end = s_offset + elems");
  __writes("for k in d_offset..d_end -> &dest[MINDEX1(d_all, k)] ~> Cell");
  __reads("for k in s_offset..s_end -> &src[MINDEX1(s_all, k)] ~> Cell");
  __admitted();

  size_t dest2 = (size_t)(dest) + d_offset*bytes_per_item;
  size_t src2 = (size_t)(src) + s_offset*bytes_per_item;
  memcpy((void*)(dest2), (void*)(src2), elems*bytes_per_item);
}

/* ---- Ghosts ---- */

__GHOST(close_wand) {
  /* LATER: Replace that id with a generated name on the respective open */
  __requires("H1: formula, H2: formula");
  __consumes("Wand(H1, H2), H1");
  __produces("H2");
  __admitted();
}

__GHOST(hide) {
  __requires("H: formula");
  __consumes("H");
  __ensures("H2: formula");
  __produces("Wand(H2, H), H2");
  __admitted();
}

__GHOST(hide_rev) {
  __reverts(hide);
  __ghost(close_wand, "");
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

/* ---- In Range ---- */

__GHOST(in_range_extend) {
  __requires("x: int, r1: range, r2: range");
  __requires("in_range(x, r1), is_subrange(r1, r2)");
  __ensures("in_range(x, r2)");
  __admitted();
}

__GHOST(in_range_shift) {
  __requires("x: int, k: int, a: int, b: int, s: int");
  __requires("in_range(x, range(a, b, s))");
  __ensures("in_range(x+k, range(a+k, b+k, s))");
  __admitted();
}

__GHOST(in_range_shift_extend) {
  __requires("x: int, k: int, r: range, a: int, b: int, s: int");
  __requires("in_range(x, range(a, b, s))");
  __requires("is_subrange(range(a+k, b+k, s), r)");
  __ensures("in_range(x+k, r)");

  __ghost(in_range_shift, "x, k, a, b, s");
  __ghost(in_range_extend, "x+k, range(a+k, b+k, s), r");
}

/* ---- Group Ghosts ---- */

__GHOST(ro_fork_group) {
  __requires("f: _Fraction, H: formula, r: range");
  __consumes("_RO(f, H)");
  // TODO: can we write: for _ in r -> _RO(f / range_count(r), H) ?
  __produces("_RO(f / range_count(r), for _ in r -> H)");
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
  __produces("for i in range -> _RO(f, items(i))");
  __admitted();
}

__GHOST(ro_factorize_group) {
  __reverts(ro_distribute_group)
  __admitted();
}
*/

__GHOST(swap_groups) {
  __requires("items: int * int -> formula, inner_range: range, outer_range: range");
  __consumes("for i in outer_range -> for j in inner_range -> items(i,j)");
  __produces("for j in inner_range -> for i in outer_range -> items(i,j)");
  __admitted();
}

__GHOST(swap_groups_rev) {
  __reverts(swap_groups);
  __admitted();
  // Buggy because reusing var ids...
  //__ghost(swap_groups, "items := fun i,j -> items(j,i)");
}

__GHOST(ro_swap_groups) {
  __requires("items: int * int -> formula, inner_range: range, outer_range: range, f: _Fraction");
  __consumes("_RO(f, for i in outer_range -> for j in inner_range -> items(i,j))");
  __produces("_RO(f,for j in inner_range -> for i in outer_range -> items(i,j))");
  __admitted();
}

__GHOST(ro_swap_groups_rev) {
  __reverts(ro_swap_groups);
  __admitted();
}

__GHOST(uninit_swap_groups) {
  __requires("items: int * int -> formula, inner_range: range, outer_range: range");
  __consumes("_Uninit(for i in outer_range -> for j in inner_range -> items(i,j))");
  __produces("_Uninit(for j in inner_range -> for i in outer_range -> items(i,j))");
  __admitted();
}

__GHOST(uninit_swap_groups_rev) {
  __reverts(uninit_swap_groups);
  __admitted();
}

__GHOST(tiled_index_in_range) {
  __requires("tile_index: int, index: int");
  __requires("tile_count: int, tile_size: int, size: int");
  __requires("size = tile_size * tile_count");
  __requires("in_range(tile_index, 0..tile_count)");
  __requires("in_range(index, 0..tile_size)");
  __ensures("in_range(tile_index * tile_size + index, 0..size)");
  __admitted();
}

__GHOST(tile_divides) {
  __requires(
    "tile_count: int, tile_size: int,"
    "size: int, items: int -> resource,"
    "bound_check: size = tile_size * tile_count"
  );
  __consumes("Group(0..size, items)");
  __produces("for bi in 0..tile_count ->"
               "for i in 0..tile_size -> items(bi * tile_size + i)");
  __admitted();
}

__GHOST(untile_divides) {
  __reverts(tile_divides);
  __admitted();
}

__GHOST(ro_tile_divides) {
  __requires(
    "tile_count: int, tile_size: int,"
    "size: int, items: int -> resource,"
    "bound_check: size = tile_size * tile_count,"
    "f: _Fraction"
  );
  __consumes("_RO(f, Group(0..size, items))");
  __produces("_RO(f, for bi in 0..tile_count ->"
               "for i in 0..tile_size -> items(bi * tile_size + i))");
  __admitted();
}

__GHOST(ro_untile_divides) {
  __reverts(ro_tile_divides);
  __admitted();
}

__GHOST(group_focus) {
  __requires("i: int, range: range, items: int -> formula");
  __requires("bound_check: in_range(i, range)");
  __consumes("Group(range, items)");
  __produces("Wand(items(i), Group(range, items)), items(i)");
  __admitted();
}

__GHOST(group_unfocus) {
  __reverts(group_focus);

  __ghost(close_wand, "");
}

__GHOST(group_ro_focus) {
  __requires("i: int, range: range, items: int -> formula, f: _Fraction");
  __requires("bound_check: in_range(i, range)");
  __consumes("_RO(f, Group(range, items))");
  __produces("Wand(_RO(f, items(i)), _RO(f, Group(range, items))), _RO(f, items(i))");
  __admitted();
}

__GHOST(group_ro_unfocus) {
  __reverts(group_ro_focus);

  __ghost(close_wand, "");
}

__GHOST(group_focus_subrange) {
  __requires("sub_range: range, big_range: range, items: int -> formula");
  __requires("bound_check: is_subrange(sub_range, big_range)");
  __consumes("Group(big_range, items)");
  __produces("Wand(Group(sub_range, items), Group(big_range, items)), Group(sub_range, items)");
  __admitted();
}

__GHOST(group_unfocus_subrange) {
  __reverts(group_focus_subrange);
  __ghost(close_wand, "");
}

__GHOST(group_focus_subrange_uninit) {
  __requires("sub_range: range, big_range: range, items: int -> formula");
  __requires("bound_check: is_subrange(sub_range, big_range)");
  __consumes("_Uninit(Group(big_range, items))");
  __produces(
    "Wand(_Uninit(Group(sub_range, items)),_Uninit(Group(big_range, items))),"
    "_Uninit(Group(sub_range, items))");
  __admitted();
}

__GHOST(group_unfocus_subrange_uninit) {
  __reverts(group_focus_subrange_uninit);
  __ghost(close_wand, "");
}

__GHOST(group2_focus_subrange_uninit) {
  __requires("outer_range: range, sub_range: range, big_range: range, items: int -> int -> formula");
  __requires("bound_check: is_subrange(sub_range, big_range)");
  __consumes("_Uninit(for i in outer_range -> Group(big_range, items(i)))");
  __produces("Wand("
      "_Uninit(for i in outer_range -> Group(sub_range, items(i))),"
      "_Uninit(for i in outer_range -> Group(big_range, items(i)))"
    "),"
    "_Uninit(for i in outer_range -> Group(sub_range, items(i)))");

  __admitted();
  /* for (int i = ra1; i < rb1; i += rs1) {
    __ghost(group_focus_subrange_uninit, ...);
  } */

  /* FIXME: This ghost should not be needed but to circumvent it we would have to write:
  __with_reverse(
    [&]() {
      __consumes("_Uninit(Group(0..10, fun i -> "
                 "Group(0..10, fun j -> "
                 "for k in 0..4 ->"
                 "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell)))");
      __produces("_Uninit(Group(0..10, fun i -> "
                 "Group(2..10, fun j -> "
                 "for k in 0..4 ->"
                 "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell)))"
                 ", "
                 "Group(0..10, fun i -> Wand("
                 "  _Uninit("
                 "  Group(2..10, fun j -> "
                 "  Group(0..4
                 "    &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))), "
                 "  _Uninit("
                 "  Group(0..10, fun j -> "
                 "  for k in 0..4 -> "
                 "    &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))"
                 "))");
      for (int i = 0; i < 10; i++) {
        __consumes("_Uninit("
                  "Group(0..10, fun j -> "
                  "for k in 0..4 ->"
                  "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))");
        __produces("_Uninit("
                  "Group(2..10, fun j -> "
                  "for k in 0..4 ->"
                  "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))"
                  ", "
                  "Wand("
                  "  _Uninit("
                  "  Group(2..10, fun j -> "
                  "  Group(0..4, fun k -> "
                  "    &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))), "
                  "  _Uninit("
                  "  Group(0..10, fun j -> "
                  "  for k in 0..4 -> "
                  "    &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))"
                  ")");
        __ghost(group_focus_subrange_uninit,
          "items := fun j -> for k in 0..4 -> &a[MINDEX3(10,10,4,i,j,k)] ~> Cell, "
          "start := 2, stop := 10, step := 1");
      }
    },
    [&]() {
      for (int i = 0; i < 10; i++) {
        __produces("_Uninit("
                  "Group(0..10, fun j -> "
                  "for k in 0..4 ->"
                  "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))");
        __consumes("_Uninit("
                  "Group(2..10, fun j -> "
                  "for k in 0..4 ->"
                  "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))"
                  ", "
                  "Wand("
                  "  _Uninit("
                  "  Group(2..10, fun j -> "
                  "  Group(0..4, fun k -> "
                  "    &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))), "
                  "  _Uninit("
                  "  Group(0..10, fun j -> "
                  "  for k in 0..4 -> "
                  "    &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))"
                  ")");
        __ghost(group_unfocus_subrange_uninit, "items := fun j -> for k in 0..4 -> &a[MINDEX3(10,10,4,i,j,k)] ~> Cell");
      }
    }
  )
  */
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
  __requires("start: int, stop: int, step: int, items: int -> formula");
  __requires("shift: int, new_start: int, new_stop: int");
  __requires("check_start: new_start = start + shift, check_stop: new_stop = stop + shift");
  __consumes("for i in range(start, stop, step) -> items(i)");
  __produces("for i in range(new_start, new_stop, step) -> items(i - shift)");
  __admitted();
}

__GHOST(group_unshift) {
  __reverts(group_shift);
  __admitted();
}

__GHOST(group_shift_uninit) {
  __requires("start: int, stop: int, step: int, items: int -> formula");
  __requires("shift: int, new_start: int, new_stop: int");
  __requires("check_start: new_start = start + shift, check_stop: new_stop = stop + shift");
  __consumes("_Uninit(for i in range(start, stop, step) -> items(i))");
  __produces("_Uninit(for i in range(new_start, new_stop, step) -> items(i - shift))");
  __admitted();
}

__GHOST(group_unshift_uninit) {
  __reverts(group_shift_uninit);
  __admitted();
}

__GHOST(group_shift_ro) {
  __requires("start: int, stop: int, step: int, items: int -> formula");
  __requires("shift: int, new_start: int, new_stop: int");
  __requires("check_start: new_start = start + shift, check_stop: new_stop = stop + shift");
  __requires("f: _Fraction");
  __consumes("_RO(f, for i in range(start, stop, step) -> items(i))");
  __produces("_RO(f, for i in range(new_start, new_stop, step) -> items(i - shift))");
  __admitted();
}

__GHOST(group_unshift_ro) {
  __reverts(group_shift_ro);
  __admitted();
}

/* ---- Matrix Ghosts ---- */

__GHOST(matrix2_focus)  {
  __requires("M: ptr, i: int, j: int, m: int, n: int");
  __requires("bound_check_i: in_range(i, 0..m)");
  __requires("bound_check_j: in_range(j, 0..n)");
  __consumes("M ~> Matrix2(m, n)");
  __produces("Wand(&M[MINDEX2(m, n, i, j)] ~> Cell, M ~> Matrix2(m, n)), &M[MINDEX2(m, n, i, j)] ~> Cell");

  __ghost(group_focus, "i := i, bound_check := bound_check_i");
  __ghost(group_focus, "i := j, bound_check := bound_check_j");
  __ghost(wand_simplify, "");
}

__GHOST(matrix2_unfocus) {
  __reverts(matrix2_focus);

  __ghost(close_wand, "");
}

__GHOST(matrix1_ro_focus) {
  __requires("M: ptr, i: int, n: int, f: _Fraction");
  __requires("bound_check: in_range(i, 0..n)");
  __consumes("_RO(f, M ~> Matrix1(n))");
  __produces("Wand(_RO(f, &M[MINDEX1(n, i)] ~> Cell), _RO(f, M ~> Matrix1(n))), _RO(f, &M[MINDEX1(n, i)] ~> Cell)");

  __ghost(group_ro_focus, "f := f, i := i, bound_check := bound_check");
}

__GHOST(matrix1_ro_unfocus) {
  __reverts(matrix1_ro_focus);

  __ghost(close_wand, "");
}

__GHOST(matrix2_ro_focus) {
  __requires("M: ptr, i: int, j: int, m: int, n: int, f: _Fraction");
  __requires("bound_check_i: in_range(i, 0..m)");
  __requires("bound_check_j: in_range(j, 0..n)");
  __consumes("_RO(f, M ~> Matrix2(m, n))");
  __produces("Wand(_RO(f, &M[MINDEX2(m, n, i, j)] ~> Cell), _RO(f, M ~> Matrix2(m,n))), _RO(f, &M[MINDEX2(m, n, i, j)] ~> Cell)");

  __ghost(group_ro_focus, "f := f, i := i, bound_check := bound_check_i");
  __ghost(group_ro_focus, "f := f, i := j, bound_check := bound_check_j");
  __ghost(wand_simplify, "");
}

__GHOST(matrix2_ro_unfocus) {
  __reverts(matrix2_ro_focus);

  __ghost(close_wand, "");
}

// matrix*_contiguous

__GHOST(matrix2_contiguous) {
  __requires("M: ptr, a: int, b: int, n2: int, n1: int");
  __consumes("for i in a..b -> for j in 0..n1 -> "
               "&M[MINDEX2(n2, n1, i, j)] ~> Cell");
  __produces("for k in a2*n1..b2*n1 -> &M[MINDEX1(n2*n1, k)] ~> Cell");
  __admitted();
}

__GHOST(matrix3_contiguous) {
  __requires("M: ptr, a: int, b: int, n3: int, n2: int, n1: int");
  __consumes("for i3 in a..b -> for i2 in 0..n2 -> "
             "for i1 in 0..n1 -> &M[MINDEX3(n3, n2, n1, i1, i2, i3)] ~> Cell");
  __produces("for k in a3*n2*n1..b3*n2*n1 -> &M[MINDEX1(n3*n2*n1, k)] ~> Cell");
  __admitted();
}

__GHOST(mindex2_contiguous) {
  __requires("M: ptr, n2: int, i2: int, n1: int, a: int, b: int");
  __consumes("for i1 in a..b -> &M[MINDEX2(n2, n1, i2, i1)] ~> Cell");
  __produces("for k in i2*n1 + a..i2*n1 + b -> &M[MINDEX1(n2*n1, k)] ~> Cell");
  __admitted();
}

__GHOST(mindex2_contiguous_rev) {
  __reverts(mindex2_contiguous);
  __admitted();
}

__GHOST(mindex3_contiguous) {
  __requires("M: ptr, n3: int, i3: int, n2: int, i2: int, n1: int, a: int, b: int");
  __consumes("for i1 in a..b -> &M[MINDEX3(n3, n2, n1, i3, i2, i1)] ~> Cell");
  __produces("Group((i3*n2*n1 + i2*n1 + a)..(i3*n2*n1 + i2*n1 + b), "
               "fun k -> &M[MINDEX1(n3*n2*n1, k)] ~> Cell)");
  __admitted();
}

__GHOST(mindex3_contiguous_rev) {
  __reverts(mindex3_contiguous);
  __admitted();
}

__GHOST(mindex2_contiguous_uninit) {
  __requires("M: ptr, n2: int, i2: int, n1: int, a: int, b: int");
  __consumes("_Uninit(for i1 in a..b -> &M[MINDEX2(n2, n1, i2, i1)] ~> Cell)");
  __produces("_Uninit(Group((i2*n1 + a)..(i2*n1 + b), "
               "fun k -> &M[MINDEX1(n2*n1, k)] ~> Cell))");
  __admitted();
}

__GHOST(mindex2_contiguous_uninit_rev) {
  __reverts(mindex2_contiguous_uninit);
  __admitted();
}

__GHOST(mindex3_contiguous_uninit) {
  __requires("M: ptr, n3: int, i3: int, n2: int, i2: int, n1: int, a: int, b: int");
  __consumes("_Uninit(for i1 in a..b -> "
                "&M[MINDEX3(n3, n2, n1, i3, i2, i1)] ~> Cell)");
  __produces("_Uninit(Group((i3*n2*n1 + i2*n1 + a)..(i3*n2*n1 + i2*n1 + b), "
               "fun k -> &M[MINDEX1(n3*n2*n1, k)] ~> Cell))");
  __admitted();
}

__GHOST(mindex3_contiguous_uninit_rev) {
  __reverts(mindex3_contiguous_uninit);
  __admitted();
}

__GHOST(mindex2_contiguous_ro) {
  __requires("M: ptr, n2: int, i2: int, n1: int, a: int, b: int, f: _Fraction");
  __consumes("_RO(f, for i1 in a..b -> &M[MINDEX2(n2, n1, i2, i1)] ~> Cell)");
  __produces("_RO(f, Group((i2*n1 + a)..(i2*n1 + b), "
               "fun k -> &M[MINDEX1(n2*n1, k)] ~> Cell))");
  __admitted();
}

__GHOST(mindex2_contiguous_ro_rev) {
  __reverts(mindex2_contiguous_ro);
  __admitted();
}

__GHOST(mindex3_contiguous_ro) {
  __requires("M: ptr, n3: int, i3: int, n2: int, i2: int, n1: int, a: int, b: int, f: _Fraction");
  __consumes("_RO(f, for i1 in a..b -> "
                "&M[MINDEX3(n3, n2, n1, i3, i2, i1)] ~> Cell)");
  __produces("_RO(f, Group((i3*n2*n1 + i2*n1 + a)..(i3*n2*n1 + i2*n1 + b), "
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
