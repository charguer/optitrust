#include <optitrust_models.h>

void test() {
  __pure();
  int* const z_a_step2 = (int*)malloc(MSIZE3(10, 10, 5) * sizeof(int));
  int* const z_b_step2 = (int*)malloc(MSIZE3(10, 10, 5) * sizeof(int));
  for (int z_k = 0; z_k < 10; z_k++) {
    __strict();
    __xpreserves(
        "for _v5 in 0..10 -> for _v6 in 0..5 -> &z_b_step2[MINDEX3(10, 10, 5, "
        "z_k, _v5, _v6)] ~> UninitCell");
    __xpreserves(
        "for _v2 in 0..10 -> for _v3 in 0..5 -> &z_a_step2[MINDEX3(10, 10, 5, "
        "z_k, _v2, _v3)] ~> UninitCell");
    int* const z_a_step1 = &z_a_step2[MINDEX3(10, 10, 5, z_k, 0, 0)];
    int* const z_b_step1 = &z_b_step2[MINDEX3(10, 10, 5, z_k, 0, 0)];
    for (int z_i = 0; z_i < 10; z_i++) {
      __strict();
      __xpreserves(
          "for _v4 in 0..5 -> &z_b_step1[MINDEX2(10, 5, z_i, _v4)] ~> "
          "UninitCell");
      __xpreserves(
          "for _v1 in 0..5 -> &z_a_step1[MINDEX2(10, 5, z_i, _v1)] ~> "
          "UninitCell");
      int* const z_a = &z_a_step1[MINDEX2(10, 5, z_i, 0)];
      int* const z_b = &z_b_step1[MINDEX2(10, 5, z_i, 0)];
      for (int z_j0 = 0; z_j0 < 5; z_j0++) {
        __strict();
        __xwrites("&z_b[MINDEX1(5, z_j0)] ~~> 0");
        z_b[MINDEX1(5, z_j0)] = 0;
      }
      for (int z_j = 0; z_j < 5; z_j++) {
        __strict();
        __sreads("for z_j0 in 0..5 -> &z_b[MINDEX1(5, z_j0)] ~~> 0");
        __xwrites("&z_a[MINDEX1(5, z_j)] ~~> 0");
        const __ghost_fn focusB =
            __ghost_begin(ro_matrix1_focus, "matrix := z_b, i := z_j");
        z_a[MINDEX1(5, z_j)] = z_b[MINDEX1(5, z_j)];
        __ghost_end(focusB);
      }
    }
  }
  free(z_b_step2);
  free(z_a_step2);
}
