#ifndef OPTITRUST_H
#define OPTITRUST_H

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

/* now declared inlined
int MINDEX1(int N1, int i1);
int MINDEX2(int N1, int N2, int i1, int i2);
int MINDEX3(int N1, int N2, int N3, int i1, int i2, int i3);
int MINDEX4(int N1, int N2, int N3, int N4, int i1, int i2, int i3, int i4);
*/

inline int MINDEX1(int N1, int i1) {
  return i1;
}

inline int MINDEX2(int N1, int N2, int i1, int i2) {
  return i1 * N2 + i2;
}

inline int MINDEX3(int N1, int N2, int N3, int i1, int i2, int i3) {
  return i1 * N2 * N3 + i2 * N3 + i3;
}

inline int MINDEX4(int N1, int N2, int N3, int N4, int i1, int i2, int i3, int i4) {
  return i1 * N2 * N3 * N4 + i2 * N3 * N4 + i3 * N4 + i4;
}

void* CALLOC1(int N1, size_t bytes_per_item);
void* CALLOC2(int N1, int N2, size_t bytes_per_item);
void* CALLOC3(int N1, int N2, int N3, size_t bytes_per_item);
void* CALLOC4(int N1, int N2, int N3, int N4, size_t bytes_per_item);

void* MALLOC1(int N1, size_t bytes_per_item);
void* MALLOC2(int N1, int N2, size_t bytes_per_item);
void* MALLOC3(int N1, int N2, int N3, size_t bytes_per_item);
void* MALLOC4(int N1, int N2, int N3, int N4, size_t bytes_per_item);


void* MALLOC_ALIGNED1(size_t N1, size_t bytes_per_item, size_t alignment);

void* MALLOC_ALIGNED2(size_t N1, size_t N2, size_t bytes_per_item, size_t alignment);

void* MALLOC_ALIGNED3(size_t N1, size_t N2, size_t N3, size_t bytes_per_item, size_t alignment);

void* MALLOC_ALIGNED4(size_t N1, size_t N2, size_t N3, size_t N4, size_t bytes_per_item, size_t alignment);

void MFREE(void* p);
void MFREE1(int N1, void* p);
void MFREE2(int N1, int N2, void* p);

bool ANY_BOOL();
int ANY(int maxValue);

#endif



