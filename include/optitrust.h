#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>

int MINDEX0() {
  return 0;
}

int MINDEX1(int N1, int i1) {
  return i1;
}

int MINDEX2(int N1, int N2, int i1, int i2) {
  return i1 * N2 + i2;
}

int MINDEX3(int N1, int N2, int N3, int i1, int i2, int i3) {
  return i1 * N2 * N3 + i2 * N3 + i3;
}

int MINDEX4(int N1, int N2, int N3, int N4, int i1, int i2, int i3, int i4) {
  return i1 * N2 * N3 * N4 + i2 * N3 * N4 + i3 * N4 + i4;
}

void* CALLOC1(int N1, size_t bytes_per_item) {
  return calloc(N1, bytes_per_item);
}

void* CALLOC2(int N1, int N2, size_t bytes_per_item) {
  return calloc(N1 * N2, bytes_per_item);
}

void* CALLOC3(int N1, int N2, int N3, size_t bytes_per_item) {
  return calloc(N1 * N2 * N3, bytes_per_item);
}

void* CALLOC4(int N1, int N2, int N3, int N4, size_t bytes_per_item) {
  return calloc(N1 * N2 * N3 * N4, bytes_per_item);
}

void* MALLOC0(size_t bytes_per_item) {
  return malloc(bytes_per_item);
}

void* MALLOC1(int N1, size_t bytes_per_item) {
  return malloc(N1 * bytes_per_item);
}

void* MALLOC2(int N1, int N2, size_t bytes_per_item) {
  return malloc(N1 * N2 * bytes_per_item);
}

void* MALLOC3(int N1, int N2, int N3, size_t bytes_per_item) {
  return malloc(N1 * N2 * N3 * bytes_per_item);
}

void* MALLOC4(int N1, int N2, int N3, int N4, size_t bytes_per_item) {
  return malloc(N1 * N2 * N3 * N4 * bytes_per_item);
}


void MFREE(void* p) {
  free(p);
}

// Potential later use

void MFREE1(int N1, void* p) {
  MFREE(p);
}

void MFREE2(int N1, int N2, void* p) {
  MFREE(p);
}


int ANY(int maxValue) { return 0; }

int exact_div(int n, int b) {
  return n / b;
}


void __requires(const char*) {}
void __ensures(const char*) {}
void __invariant(const char*) {}
void __reads(const char*) {}
void __modifies(const char*) {}
void __consumes(const char*) {}
void __produces(const char*) {}
void __independantly_modifies(const char*) {}

/*
template<typename T> T __with_ghost(T, const char*);
*/
