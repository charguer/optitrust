#include <optitrust.h>

void alloc_free_cell() {
  __pure();
  int* const p = (int*)malloc(sizeof(int));
  *p = 5;
  free(p);
}

void new_delete_cell() {
  __pure();
  int* const p = new int(5);
  free(p);
}

void alloc_free_multiple_cells() {
  __pure();
  int* const p1 = (int*)malloc(sizeof(int));
  int* const p2 = (int*)malloc(sizeof(int));
  int* const p3 = (int*)malloc(sizeof(int));
  free(p2);
  free(p1);
  free(p3);
}

int* alloc_cell() {
  __produces("_Res ~> Cell");
  __produces("Free(_Res, _Res ~> UninitCell)");
  int* const p = (int*)malloc(sizeof(int));
  *p = 5;
  return p;
}

int* alloc_multiple_cells_ret_one() {
  __produces("_Res ~> UninitCell");
  __produces("Free(_Res, _Res ~> UninitCell)");
  int* const p1 = (int*)malloc(sizeof(int));
  int* const p2 = (int*)malloc(sizeof(int));
  int* const p3 = (int*)malloc(sizeof(int));
  free(p1);
  free(p3);
  return p2;
}

void alloc_free_matrix() {
  __pure();
  int* const p = (int*)malloc(MSIZE1(2) * sizeof(int));
  free(p);
}

int* alloc_matrix() {
  __produces("_Res ~> UninitMatrix1(2)");
  __produces("Free(_Res, _Res ~> UninitMatrix1(2))");
  int* const p = (int*)malloc(MSIZE1(2) * sizeof(int));
  return p;
}

void new_delete_matrix() {
  __pure();
  int* const p = (int*)malloc(MSIZE1(2) * sizeof(int));
  free(p);
}

void alloc_free_multiple_matrices() {
  __pure();
  int* const p1 = (int*)malloc(MSIZE1(4) * sizeof(int));
  int* const p2 = (int*)malloc(MSIZE2(4, 5) * sizeof(int));
  int* const p3 = (int*)malloc(MSIZE3(4, 5, 6) * sizeof(int));
  free(p2);
  free(p1);
  free(p3);
}
