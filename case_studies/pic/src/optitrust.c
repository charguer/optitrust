#include "optitrust.h"

/* declared inline in the .h

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

*/

void* MCALLOC1(int N1, size_t bytes_per_item) {
  return calloc(N1, bytes_per_item);
}

void* MCALLOC2(int N1, int N2, size_t bytes_per_item) {
  return calloc(N1 * N2, bytes_per_item);
}

void* MCALLOC3(int N1, int N2, int N3, size_t bytes_per_item) {
  return calloc(N1 * N2 * N3, bytes_per_item);
}

void* MCALLOC4(int N1, int N2, int N3, int N4, size_t bytes_per_item) {
  return calloc(N1 * N2 * N3 * N4, bytes_per_item);
}

void* MMALLOC1(int N1, size_t bytes_per_item) {
  return malloc(N1 * bytes_per_item);
}

void* MMALLOC2(int N1, int N2, size_t bytes_per_item) {
  return malloc(N1 * N2 * bytes_per_item);
}

void* MMALLOC3(int N1, int N2, int N3, size_t bytes_per_item) {
  return malloc(N1 * N2 * N3 * bytes_per_item);
}

void* MMALLOC4(int N1, int N2, int N3, int N4, size_t bytes_per_item) {
  return malloc(N1 * N2 * N3 * N4 * bytes_per_item);
}

// void* malloc_aligned(size_t nb, size_t bytes_per_item, size_t alignment) { 
//   void* c;
//   if (posix_memalign((void**)&c, alignment, nb * bytes_per_item)) {
//    fprintf(stderr, "posix_memalign failure.\n");
//    exit(1);
//   }
  
//   return c;
// }
void* malloc_aligned (size_t nb, size_t bytes_per_item, size_t alignment){
  return aligned_alloc(alignment, nb * bytes_per_item);
}


void* MMALLOC_ALIGNED1(size_t N1, size_t bytes_per_item, size_t alignment) {
  return malloc_aligned(N1, bytes_per_item, alignment);
}

void* MMALLOC_ALIGNED2(size_t N1, size_t N2, size_t bytes_per_item, size_t alignment) {
  return malloc_aligned(N1 * N2, bytes_per_item, alignment);
}

void* MMALLOC_ALIGNED3(size_t N1, size_t N2, size_t N3, size_t bytes_per_item, size_t alignment) {
  return malloc_aligned(N1 * N2 * N3, bytes_per_item, alignment);
}

void* MMALLOC_ALIGNED4(size_t N1, size_t N2, size_t N3, size_t N4, size_t bytes_per_item, size_t alignment) {
  return malloc_aligned(N1 * N2 * N3 * N4, bytes_per_item, alignment);
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

bool ANY_BOOL () {
  return true;
}

int ANY(int maxValue) {
  return 0;
}
