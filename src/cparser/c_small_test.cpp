
#include "stdlib.h"

int main() {
  int nb = 5;
  int* t = (int*) malloc(nb*sizeof(int));
}

// gcc -std=c99 c_small_test.cpp
// gcc -std=c++11 c_small_test.cpp