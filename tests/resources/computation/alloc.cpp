#include <optitrust.h>

void alloc_free_cell() {
  __pure();
  int* const p = MALLOC(int);
  *p = 5;
  free(p);
  //free(p); //should fail if uncommented
}

void new_delete_cell() {
  __pure();
  int* const p = new int(5);
  delete p;
}

void alloc_free_multiple_cells() {
  __pure();
  int* const p1 = MALLOC(int);
  int* const p2 = MALLOC(int);
  int* const p3 = MALLOC(int);
  free(p2);
  free(p1);
  free(p3);
}

int* alloc_cell() {
  __produces("_Res ~> Cell, _Free(_Res, _Res ~> Cell)");
  int* const p = MALLOC(int);
  *p = 5;
  return p;
}

int* alloc_multiple_cells_ret_one() {
  __produces("_Uninit(_Res ~> Cell), _Free(_Res, _Res ~> Cell)");
  int* const p1 = MALLOC(int);
  int* const p2 = MALLOC(int);
  int* const p3 = MALLOC(int);
  free(p1);
  free(p3);
  return p2;
}

// Cannot typecheck without models
/*void mut_ptr() {
  __pure();
  int* p;
  p = MALLOC(int);
  free(p);
}

void realloc_cell() {
  __pure();
  int* p = MALLOC(int);
  free(p);
  p = MALLOC(int);
  free(p);
}*/

void alloc_free_matrix() {
  __pure();
  int* const p = MALLOC1(int, 2);
  free(p);
  //free(p); //should fail if uncommented
}

int* alloc_matrix() {
  __produces("_Uninit(_Res ~> Matrix1(2)), _Free(_Res, _Res ~> Matrix1(2))");
  int* const p = MALLOC1(int, 2);
  return p;
}

void new_delete_matrix() {
  __pure();
  int* const p = new int[MSIZE1(2)];
  delete[] p;
}

void alloc_free_multiple_matrices() {
  __pure();
  int* const p1 = MALLOC1(int, 4);
  int* const p2 = MALLOC2(int, 4, 5);
  int* const p3 = MALLOC3(int, 4, 5, 6);
  free(p2);
  free(p1);
  free(p3);
}
