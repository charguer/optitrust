#include <string.h>

void p1(char ** a1) {
  char * b1 = NULL;
  b1 = a1[0];
  b1 = strcpy(b1, a1[1]);
}

void p2(int ** a2, int * d2) {
  int * b2 = a2[1], * c2 = NULL;
  *b2 = 0;
  c2 = *a2;
  (*c2)++;
  c2 = d2;
}

void p3(char** a3) {
  char* b3 = NULL;
  b3 = a3[0];
  b3[0]++;
}

void p4(int ** a4) {
  int * b4 = a4[1];
  *b4 = 0;
}

int * p5(int ** a5) {
  int * b5 = a5[1];
  return b5;
}

int ** p6(int ** a6) {
  return a6;
}

int * p7(int ** a7) {
  return a7[1];
}

int * p8(int ** a8) {
  return *a8;
}

int p9(int ** a9) {
  return **a9;
}
