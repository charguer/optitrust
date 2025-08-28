#include <cmath>
#include <optitrust.h>
const int GS = 32;

void accesses(float *v, int n) {
  __reads("v ~> Matrix1(n)");
  float b = 0.f;
  float a = 0.f;
  for (int i = 0; i < n; i++) {
    __GHOST_BEGIN(f, ro_matrix1_focus,"v,i");
    a = a + v[MINDEX1(n,i)];
    __GHOST_END(f);
  }
  b = b + a;
}

// local name
// void accesses2(float *v, int n) {
//   float *w = MALLOC1(float, n);
//   for (int i = 0; i < n; i++) {
//     w[i] = v[i];
//   }
//   float b = 0.f;
//   float a = 0.f;
//   for (int i = 0; i < n; i++) {
//     a = a + w[i];
//   }
//   b = b + a;
// }

// // insert max + inv max
// void accesses3(float *v, int n) {
//   float s = 0;
//   for (int i = 0; i < n; i++) {
//     float s = max(s, v[i]);
//   }
//   float inv_s = 1.0f / s;
//   float *w = MALLOC1(float, n);
//   for (int i = 0; i < n; i++) {
//     w[i] = v[i];
//   }
//   float b = 0.f;
//   float a = 0.f;
//   for (int i = 0; i < n; i++) {
//     a = a + w[i];
//   }
//   b = b + a;
// }

// // scale w -> NOT SO SURE IT WORKS
// void accesses4(float *v, int n) {
//   float s = 0;
//   for (int i = 0; i < n; i++) {
//     float s = max(s, v[i]);
//   }
//   float inv_s = 1.0f / s;
//   float *w = MALLOC1(float, n);
//   for (int i = 0; i < n; i++) {
//     w[i] = v[i] * inv_s;
//   }
//   float b = 0.f;
//   float a = 0.f;
//   for (int i = 0; i < n; i++) {
//     a = a + exact_div(w[i], inv_s);
//   }
//   b = b + a;
// }

// // scale a
// void accesses5(float *v, int n) {
//   float s = 0;
//   for (int i = 0; i < n; i++) {
//     float s = max(s, v[i]);
//   }
//   float inv_s = 1.0f / s;
//   float *w = MALLOC1(float, n);
//   for (int i = 0; i < n; i++) {
//     w[i] = v[i] * inv_s;
//   }
//   float b = 0.f;
//   float a = 0.f * inv_s;
//   for (int i = 0; i < n; i++) {
//     a = (exact_div(a, inv_s) + exact_div(w[i], inv_s)) * inv_s;
//   }
//   b = b + exact_div(a, inv_s);
// }

// // arith_simple
// void accesses6(float *v, int n) {
//   float s = 0;
//   for (int i = 0; i < n; i++) {
//     float s = max(s, v[i]);
//   }
//   float inv_s = 1.0f / s;
//   float *w = MALLOC1(float, n);
//   for (int i = 0; i < n; i++) {
//     w[i] = v[i] * inv_s;
//   }
//   float b = 0.f;
//   float a = 0.f * inv_s;
//   for (int i = 0; i < n; i++) {
//     a = a + w[i];
//   }
//   b = b + exact_div(a, inv_s);
// }


// // cast w
// void accesses7(float *v, int n) {
//   float s = 0;
//   for (int i = 0; i < n; i++) {
//     float s = max(s, v[i]);
//   }
//   float inv_s = 1.0f / s;
//   int *w = MALLOC1(int , n);
//   for (int i = 0; i < n; i++) {
//     w[i] = (int) v[i] * inv_s;
//   }
//   float b = 0.f;
//   float a = 0.f * inv_s;
//   for (int i = 0; i < n; i++) {
//     a = a + (float) w[i];
//   }
//   b = b + exact_div(a, inv_s);
// }


// // cast a
// void accesses8(float *v, int n) {
//   float s = 0;
//   for (int i = 0; i < n; i++) {
//     float s = max(s, v[i]);
//   }
//   float inv_s = 1.0f / s;
//   int *w = MALLOC1(int , n);
//   for (int i = 0; i < n; i++) {
//     w[i] = (int) v[i] * inv_s;
//   }
//   float b = 0.f;
//   int a = (int) 0.f * inv_s;
//   for (int i = 0; i < n; i++) {
//     a = (int) (float) a + (float) w[i];
//   }
//   b = b + exact_div( (float) a, inv_s);
// }


// // rewrite casts
// void accesses9(float *v, int n) {
//   float s = 0;
//   for (int i = 0; i < n; i++) {
//     float s = max(s, v[i]);
//   }
//   float inv_s = 1.0f / s;
//   int *w = MALLOC1(int , n);
//   for (int i = 0; i < n; i++) {
//     w[i] = (int) v[i] * inv_s;
//   }
//   float b = 0.f;
//   int a = (int) 0.f * inv_s;
//   for (int i = 0; i < n; i++) {
//     a =  a +  w[i];
//   }
//   b = b + exact_div( (float) a, inv_s);
// }
