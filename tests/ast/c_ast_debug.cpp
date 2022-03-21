int main(void)
{
  int count = 0;
  #pragma omp parallel for
  for (int n = 0; n < 8; n++) {
    #pragma omp atomic
    count++;
  }
  #pragma omp parallel for collapse(2)
  for (int a = 0; a < 8; a++) {
    for (int b = 0; b < 8; b++) {
    #pragma omp atomic
    count++;
    }
  }
  return 0;
}


/*

typedef struct { int x; int y; } vect;
typedef struct { vect pos; } particle;

int main() {
  particle v = { { 2, 3 } };
  //vect v = { 2, 4 };
  // return v.x;
}
*/

/*
// TODO: move this to a unit test in ast/ folder on arrays
typedef struct {
  int t[2];
} foo;

int main() {
  const foo f = { { 0, 1 } };
  const int * const u = f.t;
  int const a = u[0];
  int b = a;
}
*/

/*
TODO: try this  in the unit test for simpl_accesses

define vect

int test() {
  vect w = {0,1};
  const int a = w.x; //  get(struct_accesses(w,x))

  const vect v = {0, 1};
  const int a = v.x; // struct_get(v,x)
}


TODO: try the transfo that makes v nonconst.
  replaces v with get(v)

  const int a = get(v).x; // now have struct_get(get(v),x)
  // a separate transfo:
  // Expr.simpl_accesses will give   get(struct_accesses(v,x))

*/

/* TODO: move this function to another unit test in ast/c_ast.cpp, merge with test_loop
void test_loop() {

  int k;
  int a = 0;
  for (k = 0; k < 10; k++) {
    a += k;
  }
  for (int i = 0; i <= 10; i++) {
      i++;
  }
  for (int i = 0; i < 10; i++) {
      i++;
  }
  for (int j = 10; j >= 0; j--) {
      j++;
  }
  for (int j = 10; j > 0; j--) {
      j++;
  }

}
*/