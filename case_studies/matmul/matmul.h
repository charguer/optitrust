#pragma once

/* Multiplies the matrix A (dim m x p) by the matrix B (dim p x n),
 * and writes the result in the matrix C (dim m x n):
 *   C = A * B
 */
// void mm(float* C, float* A, float* B, int m, int n, int p);

// m = n = p = 1024
void mm1024(float* C, float* A, float* B);