#include <optitrust_models.h>

// triangle(j) = 0 + 1 + ... + (j - 1)
__DECL(triangle, "int -> int");
__AXIOM(triangle_zero, "0 = triangle(0)");
__AXIOM(triangle_succ, "forall (j: int) -> triangle(j) + j = triangle(j + 1)");

void ok1() {
  __pure();

  int a = 0;
  __ghost(rewrite_linear, "inside := fun v -> &a ~~> v, by := triangle_zero");
  for (int j = 0; j < 10; j++) {
    __spreserves("&a ~~> triangle(j)");
    __ghost(rewrite_linear, "inside := fun v -> &a ~~> v, by := plus_zero_intro(triangle(j))");
    for (int i = 0; i < j; i++) {
      __spreserves("&a ~~> triangle(j) + i");
      a++;
      __ghost(rewrite_linear, "inside := fun v -> &a ~~> v, by := add_assoc_right(triangle(j), i, 1)");
    }
    __ghost(rewrite_linear, "inside := fun v -> &a ~~> v, by := triangle_succ(j)");
  }
}


void ok2() {

  int a = 0;
  l: {
    a++;
  }

  int y = 0;
}

void ko1() {
  int a = 0;
  int& b = a;
  for (int j = 0; j < 10; j++) {
    for (int i = 0; i < j; i++) {
      a++;
      b++;
    }
  }
}

//   int y = 0;
// }

// void ko2() {
//   __pure();

//   int a = 0;
//   int& b = a;
//   l: {
//     a++;
//     b++;
//   }

//   int y = 0;
// }

// void ko_scope() {
//   __pure();
//   int x = 0;
//   int a = 0;
//   l: { a++; }
// }

// void ok3() {
//   __pure();
//   int a = 0;
//   for (int i = 0; i < 10; i++) {
//     a++;
//   }
// }

// void ok4() {
//   __pure();

//   int a = 0;
//   /*@ target__begin @*/
//   int b = 0;
//   a++;
//   /*@ target__end @*/
//   b++;
// }
//
