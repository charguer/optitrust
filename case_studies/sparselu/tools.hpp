#ifndef __TOOLS_HPP
#define __TOOLS_HPP

#include <stdio.h>
#include <stdlib.h>

float ** genmat(float ** matrix, size_t matrix_size, size_t submatrix_size);
int store_structure(
  const char * output, const char * name, float ** matrix, size_t matrix_size
);
int store_matrix(
  const char * output, const char * name,
  float ** matrix, size_t matrix_size, size_t submatrix_size
);

#endif // __TOOLS_HPP