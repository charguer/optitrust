#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <assert.h>
#include <string.h>

/* ---- Resource Annotations ---- */

inline void __pure() {}
inline void __requires(const char*) {}
inline void __ensures(const char*) {}
inline void __invariant(const char*) {}
inline void __reads(const char*) {}
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
typedef __ghost_ret (*__ghost_fn)();

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

inline void __reverts(__ghost_fn) {}

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
  __modifies("p ~> Cell");
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
  __produces("_Res ~> Matrix0()");
  __admitted();
  return malloc(bytes_per_item);
}

inline void* MALLOC1(int N1, size_t bytes_per_item) {
  __produces("_Res ~> Matrix1(N1)");
  __admitted();
  return malloc(N1 * bytes_per_item);
}

inline void* MALLOC2(int N1, int N2, size_t bytes_per_item) {
  __produces("_Res ~> Matrix2(N1, N2)");
  __admitted();
  return malloc(N1 * N2 * bytes_per_item);
}

inline void* MALLOC3(int N1, int N2, int N3, size_t bytes_per_item) {
  __produces("_Res ~> Matrix3(N1, N2, N3)");
  __admitted();
  return malloc(N1 * N2 * N3 * bytes_per_item);
}

inline void* MALLOC4(int N1, int N2, int N3, int N4, size_t bytes_per_item) {
  __produces("_Res ~> Matrix4(N1, N2, N3, N4)");
  __admitted();
  return malloc(N1 * N2 * N3 * N4 * bytes_per_item);
}

inline void MFREE0(void* p) {
  __consumes("p ~> Matrix0()");
  __admitted();
  free(p);
}

inline void MFREE1(int N1, void* p) {
  __consumes("p ~> Matrix1(N1)");
  __admitted();
  free(p);
}

inline void MFREE2(int N1, int N2, void* p) {
  __consumes("p ~> Matrix2(N1, N2)");
  __admitted();
  free(p);
}

inline void MFREE3(int N1, int N2, int N3, void* p) {
  __consumes("p ~> Matrix3(N1, N2, N3)");
  __admitted();
  free(p);
}

inline void MFREE4(int N1, int N2, int N3, int N4, void* p) {
  __consumes("p ~> Matrix4(N1, N2, N3, N4)");
  __admitted();
  free(p);
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

/* ---- Group Ghosts ---- */

__GHOST(ro_fork_group) {
  __requires("f: _Fraction, R: formula, r: range");
  __consumes("_RO(f, R)");
  // TODO: can we write: Group(r, fun _ -> _RO(f / range_count(r), R)) ?
  __produces("_RO(f / range_count(r), Group(r, fun _ -> R))");
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

__GHOST(group_focus) {
  __requires("i: int, start: int, stop: int, step: int, items: int -> formula");
  __requires("bound_check_start: start <= i, bound_check_stop: i < stop, bound_check_step: (start + i) % step = 0");
  __consumes("Group(range(start, stop, step), items)");
  __produces("items(i), Wand(items(i), Group(range(start, stop, step), items))");
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
  __produces("_RO(f, items(i)), Wand(_RO(f, items(i)), _RO(f, Group(range(start, stop, step), items)))");
  __admitted();
}

__GHOST(group_ro_unfocus) {
  __reverts(group_ro_focus);
  __ghost(close_wand, "");
}

__GHOST(group_focus_subrange) {
  __requires("start: int, stop: int, step: int, old_start: int, old_stop: int, items: int -> formula");
  __requires("bound_check_start: old_start <= start, bound_check_stop: stop <= old_stop");
  __consumes("Group(range(old_start, old_stop, step), items)");
  __produces("Group(range(start, stop, step), items), Wand(Group(range(start, stop, step), items), Group(range(old_start, old_stop, step), items))");
  __admitted();
}

__GHOST(group_unfocus_subrange) {
  __reverts(group_focus_subrange);
  __ghost(close_wand, "");
}

/* ---- Matrix Ghosts ---- */

// FIXME: matrix2_*focus ghosts are not checking bounds
__GHOST(matrix2_focus)  {
  __requires("M: ptr, i: int, j: int, m: int, n: int");
  __consumes("M ~> Matrix2(m, n)");
  __produces("&M[MINDEX2(m, n, i, j)] ~> Cell, Wand(&M[MINDEX2(m, n, i, j)] ~> Cell, M ~> Matrix2(m, n))");
  __ghost(group_focus, "i := i, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
  __ghost(group_focus, "i := j, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
  __ghost(wand_simplify, "");
}

__GHOST(matrix2_unfocus) {
  __reverts(matrix2_focus);
  __ghost(close_wand, "");
}

__GHOST(matrix2_ro_focus) {
  __requires("M: ptr, i: int, j: int, m: int, n: int, f: _Fraction");
  __consumes("_RO(f, M ~> Matrix2(m, n))");
  __produces("_RO(f, &M[MINDEX2(m, n, i, j)] ~> Cell), Wand(_RO(f, &M[MINDEX2(m, n, i, j)] ~> Cell), _RO(f, M ~> Matrix2(m,n)))");
  __ghost(group_ro_focus, "f := f, i := i, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
  __ghost(group_ro_focus, "f := f, i := j, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
  __ghost(wand_simplify, "");
}

__GHOST(matrix2_ro_unfocus) {
  __reverts(matrix2_ro_focus);
  __ghost(close_wand, "");
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
