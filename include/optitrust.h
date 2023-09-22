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

// Argument type for ghost functions
typedef const char* __ghost_args;

// Marcro for ghost function prototype
#define __GHOST(f) inline __ghost_ret f()

// Invoke a ghost function
inline void __ghost(__ghost_ret (*)(), __ghost_args) {}

/// Postfix call for specifying ghost arguments
inline void __with(__ghost_args) {}
template<typename T> T __call_with(T ret_val, __ghost_args) { return ret_val; }

// TODO: bind et call_with
/*
template<typename T> T __bind(T ret_val, const char*) { return ret_val; }
inline void __rename(const char*) {}
*/

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

/* ---- Matrix Ghosts ---- */

__GHOST(close_wand) {
  /* LATER: Replace that id with a generated name on the respective open */
  __requires("wand_id: int, H1: formula, H2: formula");
  __consumes("Wand(wand_id, H1, H2), H1");
  __produces("H2");
  __admitted();
}

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
  __requires(
    "tile_count: int, tile_size: int,"
    "n: int, to_item: int -> resource,"
    "bound_check: n = tile_size * tile_count"
  );
  __consumes("Group(range(0, tile_count, 1), fun bi ->"
               "Group(range(0, tile_size, 1), fun i -> to_item(bi * tile_size + i)))");
  __produces("Group(range(0, n, 1), to_item)");
  __admitted();
}

__GHOST(matrix2_focus)  {
    __requires("M: ptr, i: int, j: int, m: int, n: int");
    __consumes("M ~> Matrix2(m, n)");
    __produces("&M[MINDEX2(m, n, i, j)] ~> Cell, M ~> FocussedMatrix2(m, n, i)");
    __admitted();
}

__GHOST(matrix2_unfocus) {
    __requires("M: ptr, m: int, n: int, i: int, j: int");
    __consumes("M ~> FocussedMatrix2(m, n, i), &M[MINDEX2(m, n, i, j)] ~> Cell");
    __produces("M ~> Matrix2(m, n)");
    __admitted();
}

__GHOST(matrix2_ro_focus) {
    __requires("M: ptr, i: int, j: int, m: int, n: int, f: _Fraction");
    __consumes("_RO(f, M ~> Matrix2(m, n))");
    __produces("_RO(f, &M[MINDEX2(m, n, i, j)] ~> Cell), _RO(f, M ~> FocussedMatrix2(m, n, i, j))");
    __admitted();
}

__GHOST(matrix2_ro_unfocus) {
    __requires("M: ptr, m: int, n: int, i: int, j: int, f: _Fraction");
    __consumes("_RO(_Full(f), M ~> FocussedMatrix2(m, n, i, j)), _RO(_Full(f), &M[MINDEX2(m, n, i, j)] ~> Cell)");
    __produces("_RO(f, M ~> Matrix2(m, n))");
    __admitted();
}

__GHOST(group_focus_subrange) {
  __requires("wand_id: int, start: int, stop: int, step: int, old_start: int, old_stop: int, items: int -> formula");
  __requires("bound_check_start: old_start <= start, bound_check_stop: stop <= old_stop");
  __consumes("Group(range(old_start, old_stop, step), items)");
  __produces("Group(range(start, stop, step), items), Wand(wand_id, Group(range(start, stop, step), items), Group(range(old_start, old_stop, step), items))");
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
