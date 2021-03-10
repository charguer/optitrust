
// Description of the encoding performed for the AST representation

typedef struct { int x; int y; } vect;

typedef struct { vect pos; vect speed; } particle;

// Function arguments are assumed to be 'const' by default
int f(int n) {
  return n;
}


void test_loop() {
  for (int i = 0; i < 10; i++) {
      i++;
     // TODO find out why i++ is not encoded?
  }

}


// Stack allocated variables are turned into heap allocated variables
void stack_var() {
  int r = 3;
  r = r + 1;
  r += 2;
  r++;
  int s = f(r);
}

// Field and arrays accessed are composed in two steps:
// first, compute the address of the memory cell, then derefence it.

// For example,  t[i]  is decomposed as the operation that computes &t[i],
// then applying the star operator to it, obtaining  "*(&t[i])".
// The star is the standard dereferencing operator.
// In the AST, the operation &t[i] is the application of the primitive
// binary operation "Binop_array_access" to the argument t and i.

void stack_array() {
  int t[2] = { 5, 6 };
  int a = t[0]; // int a = *(t + 0)
  t[1] = a + 2;
}

// Likewise,  t.x  is decomposed as *(&t.x), which involves the primitive
// unary operator "Unop_struct_access x" applied to the argument t.

void stack_struct() {
  vect v = { 5, 6 };
  int a = v.x; // TODO print quotes in struct_access(v, "x")
  v.y = a + 2;
  vect v2 = v;

  // nested structs//
  particle p1 = { v, v };
  particle p2 = { v, { 7,8 } };
}

// References: not yet implemented
/*
void references() {
  int a = 3;
  int& b = a;
  b = b + 4;
}
*/

void constants() {
  const int a = 3;
  const int b = a + 3;
  int c = b + 4;
  // TODO : const vect v = { 0, 1};
  // int a = v.x
}

typedef int* intstar;

void const_pointers() {
  int a = 3;
  const intstar b = &a;
  const int c = *b + 4;
}

void nonconst_pointers() {
  int a = 3;
  int* b = &a;
  *b = *b + 4;
  // TODO int c = 3;
  // b = &c;
}


// When arrays are passed by values, they are not heap allocated.
// They are values, out of which it is possible to directly read a value
// using the operator Binop_array_get.
// Likewise a struct passed by value is read using Unop_struct_get.
void by_value(int t[2], vect v) { // implicit const arguments
  int b = t[0];
  int a = v.x;
  // if trying to writing into t or v, should get an error, because they are assumed to be const
  // TODO: check whether we indeed see an error
  vect v2 = v; // this should make a copy of the data
}

int main() {


}