#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <assert.h>
#include <string.h>

/* ---- Resource Annotations ---- */

void __pure() {}
void __requires(const char*) {}
void __ensures(const char*) {}
void __invariant(const char*) {}
void __reads(const char*) {}
void __modifies(const char*) {}
void __consumes(const char*) {}
void __produces(const char*) {}
void __sequentially_reads(const char*) {}
void __sequentially_modifies(const char*) {}

void __admitted() {}

typedef void __ghost;

template<typename T> T __ghost_args(T ret_val, const char*) { return ret_val; }
/*
template<typename T> T __ghost_bind(T ret_val, const char*) { return ret_val; }
void __res_rename(const char*) {}
*/

/* ---- Matrix Functions ---- */

int MINDEX0() {
  __pure();
  __admitted();
  return 0;
}

int MINDEX1(int N1, int i1) {
  __pure();
  __admitted();
  return i1;
}

int MINDEX2(int N1, int N2, int i1, int i2) {
  __pure();
  __admitted();
  return i1 * N2 + i2;
}

int MINDEX3(int N1, int N2, int N3, int i1, int i2, int i3) {
  __pure();
  __admitted();
  return i1 * N2 * N3 + i2 * N3 + i3;
}

int MINDEX4(int N1, int N2, int N3, int N4, int i1, int i2, int i3, int i4) {
  __pure();
  __admitted();
  return i1 * N2 * N3 * N4 + i2 * N3 * N4 + i3 * N4 + i4;
}

void* CALLOC1(int N1, size_t bytes_per_item) {
  __produces("_Res ~> Matrix1(N1);");
  __admitted();
  return calloc(N1, bytes_per_item);
}

void* CALLOC2(int N1, int N2, size_t bytes_per_item) {
  __produces("_Res ~> Matrix2(N1, N2);");
  __admitted();
  return calloc(N1 * N2, bytes_per_item);
}

void* CALLOC3(int N1, int N2, int N3, size_t bytes_per_item) {
  __produces("_Res ~> Matrix3(N1, N2, N3);");
  __admitted();
  return calloc(N1 * N2 * N3, bytes_per_item);
}

void* CALLOC4(int N1, int N2, int N3, int N4, size_t bytes_per_item) {
  __produces("_Res ~> Matrix4(N1, N2, N3, N4);");
  __admitted();
  return calloc(N1 * N2 * N3 * N4, bytes_per_item);
}

void* MALLOC0(size_t bytes_per_item) {
  __produces("_Res ~> Matrix0();");
  __admitted();
  return malloc(bytes_per_item);
}

void* MALLOC1(int N1, size_t bytes_per_item) {
  __produces("_Res ~> Matrix1(N1);");
  __admitted();
  return malloc(N1 * bytes_per_item);
}

void* MALLOC2(int N1, int N2, size_t bytes_per_item) {
  __produces("_Res ~> Matrix2(N1, N2);");
  __admitted();
  return malloc(N1 * N2 * bytes_per_item);
}

void* MALLOC3(int N1, int N2, int N3, size_t bytes_per_item) {
  __produces("_Res ~> Matrix3(N1, N2, N3);");
  __admitted();
  return malloc(N1 * N2 * N3 * bytes_per_item);
}

void* MALLOC4(int N1, int N2, int N3, int N4, size_t bytes_per_item) {
  __produces("_Res ~> Matrix4(N1, N2, N3, N4);");
  __admitted();
  return malloc(N1 * N2 * N3 * N4 * bytes_per_item);
}

void MFREE0(void* p) {
  __consumes("p ~> Matrix0();");
  __admitted();
  free(p);
}

void MFREE1(int N1, void* p) {
  __consumes("p ~> Matrix1(N1);");
  __admitted();
  free(p);
}

void MFREE2(int N1, int N2, void* p) {
  __consumes("p ~> Matrix2(N1, N2);");
  __admitted();
  free(p);
}

void MFREE3(int N1, int N2, int N3, void* p) {
  __consumes("p ~> Matrix3(N1, N2, N3);");
  __admitted();
  free(p);
}

void MFREE4(int N1, int N2, int N3, int N4, void* p) {
  __consumes("p ~> Matrix4(N1, N2, N3, N4);");
  __admitted();
  free(p);
}

/* ---- Matrix Ghosts ---- */

__ghost ghost_matrix2_focus(float* M, int i, int j) {
    __requires("m: int; n: int;");
    __consumes("M ~> Matrix2(m, n);");
    __produces("&M[MINDEX2(m, n, i, j)] ~> Cell; M ~> FocussedMatrix2(m, n, i);");
    __admitted();
}

__ghost ghost_tile_divides() {
  __requires(
    "tile_count: int; tile_size: int;"
    "gn: int; to_item: int -> resource;"
    "__assert_eq(gn, tile_size * tile_count);"
  );
  __consumes("Group(range(0, gn, 1), to_item);");
  __produces("Group(range(0, tile_count, 1), fun bi ->"
             "Group(range(0, tile_size, 1), fun i -> to_item(bi * tile_size + i)));");
  __admitted();
}

__ghost ghost_tile_divides_reverse() {
  __requires(
    "tile_count: int; tile_size: int;"
    "gn: int; to_item: int -> resource;"
    "__assert_eq(gn, tile_size * tile_count);"
  );
  __consumes("Group(range(0, tile_count, 1), fun bi ->"
             "Group(range(0, tile_size, 1), fun i -> to_item(bi * tile_size + i)));");
  __produces("Group(range(0, gn, 1), to_item);");
  __admitted();
}

__ghost ghost_matrix2_unfocus(float* M) {
    __requires("m: int; n: int; i: int; j: int;");
    __consumes("M ~> FocussedMatrix2(m, n, i); &M[MINDEX2(m, n, i, j)] ~> Cell;");
    __produces("M ~> Matrix2(m, n);");
    __admitted();
}

__ghost ghost_matrix2_ro_focus(float* M, int i, int j) {
    __requires("m: int; n: int; f: _Fraction;");
    __consumes("_RO(f, M ~> Matrix2(m, n));");
    __produces("_RO(f, &M[MINDEX2(m, n, i, j)] ~> Cell); _RO(f, M ~> FocussedMatrix2(m, n, i, j));");
    __admitted();
}

__ghost ghost_matrix2_ro_unfocus(float* M) {
    __requires("m: int; n: int; i: int; j: int; f: _Fraction;");
    __consumes("_RO(_Full(f), M ~> FocussedMatrix2(m, n, i, j)); _RO(_Full(f), &M[MINDEX2(m, n, i, j)] ~> Cell);");
    __produces("_RO(f, M ~> Matrix2(m, n));");
    __admitted();
}


/* ---- Arithmetic Functions ---- */

int exact_div(int n, int b) {
  __pure();
  __admitted();
  return n / b;
}

/* ---- Other Functions ---- */

void MFREE(void* p) {
  free(p);
}

int ANY(int maxValue) { return 0; }
