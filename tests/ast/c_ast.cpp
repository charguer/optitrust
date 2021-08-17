
// Description of the encoding performed for the AST representation

typedef struct { int x; int y; } vect;

typedef struct { vect pos; vect speed; } particle;

typedef struct vects { vect head; vects* tail; } vects;

typedef vect vect2;
typedef vect2 vect3;
typedef int int2[2];
typedef int* intstar;

struct tree_node {
  struct tree_node *left;
  struct tree_node *right;
};
int x
void initlist() {
  vect v1 = { 1, 2 };
  vect3 v2 = { 1, 2 };
  int2 p = { 1, 2 };
  intstar n = &p[0];
}


// Function arguments are assumed to be 'const' by default
int f(int n) {
  return n;
}


void test_loop() {
  for (int i = 0; i < 10; i++) {
      i++;
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
// binary operation "Binop_array_cell_addr" to the argument t and i.

void stack_array() {
  int t[2] = { 5, 6 };
  int a = t[0]; // int a = *(t + 0)
  t[1] = a + 2;
}

// Likewise,  t.x  is decomposed  as *(&t.x), which involves the primitive
// unary operator "Unop_struct_field_addr x" applied to the argument t.

void stack_struct() {
  vect v = { 5, 6 };
  int a = v.x; 
  v.y = a + 2;
  vect v2 = v;
  v2 = v;

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
  const vect v = { 0, 1};
  int d = v.x;
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
  int c = 3;
  b = &c;
}


// When arrays are passed by values, they are not heap allocated.
// They are values, out of which it is possible to directly read a value
// using the operator Binop_array_cell_get.
// Likewise a struct passed by value is read using Unop_struct_get.
void by_value(int t[2], vect v) { // implicit const arguments
  int b = t[0];
  int a = v.x;
  vect v2 = v; // this should make a copy of the data
}

int main() {


}


int g(int x) {
  x = x + 1;
  return x;
}

int immutable_stack_ptr() {
  int x = 3;
  int y = f(x);
  const int * p = &x;
  int * const q = &x;
  const int * const r = &x;
  p = &y;
  *q = 5;
  return *p + *q + *r;
}

int immutable_stack_array() {
  int x = 3;
  int y = 4;
  int* const t[2] = { &x, &y };
  // t[0] = &y; // disallowed
  *(t[0]) = 3; // allowed, because t[0] is not const

  const int* u[2] = { &x, &y };
  u[0] = &y; // allowed, because u itself is not const
  // *(u[0]) = 3; disallowed

  const int* const v[2] = { &x, &y };
  // v[0] = &y; // disallowed
  //*(v[0]) = 3; // disallowed
  return *(t[0]);
}

int immutable_stack_var() {
  int const a = 4;
  const int r = 3;
  const int s = r + 1;
  return r;
}

int mutable_stack_var() {
  int r = 3;
  r = r + 1;
  r++;
  return r;
}

int mutable_stack_array (){
  int x = 3;
  int y = 4;
   int* w[2] = { &x, &y };
   w[0] = &y; // allowed
   *(w[0]) = 3; // allowed
   return *(w[0]);
}
// void advanced_stuff () {
//     int* x;
//     int ** p;
//     int ** const q = &x;
//     int * const * r = &
//     int const ** s;
//     int * const * const u;

// }

/*  ENCODED VERSION
int mutable_stack_var() {
  const int* r = new 3;
  set(r, get(r) + 1);
  operator++(r);
  return get(r);
}
*/




/*
  t[3].f
  t[3]->f
  t->f[3]
  t.f[3]

  both in case t a stack allocated
  or t in a const
  or t is a function argument passed by pointer
  or t is a  function argument passed by value


*/





