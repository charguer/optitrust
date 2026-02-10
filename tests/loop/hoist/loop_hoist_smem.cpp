#include <optitrust_models.h>

void crap() {
  for (int z_k = 0; z_k < 10; z_k++) {
    for (int z_i = 0; z_i < 10; z_i++) {
      int * const z_a = MALLOC1(int, 5);
      int * const z_b = MALLOC1(int, 5);
      for (int z_j0; z_j0 < 5; z_j0++) {
        __xwrites("&z_a[MINDEX1(5,z_j)] ~~> 0");
        z_b[MINDEX1(5,0)] = 0;
      }
      for (int z_j; z_j < 5; z_j++) {
        __xwrites("&z_a[MINDEX1(5,z_j)] ~~> 0");
        __GHOST_BEGIN(focusB, ro_matrix1_focus, "z_b, z_j");
        z_a[MINDEX1(5, z_j)] = z_b[MINDEX1(5,0)];
        __GHOST_END(focusB);
      }
      free(z_a);
      free(z_b);
    }
  }
}
