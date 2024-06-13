
typedef long unsigned int size_t;

typedef int myint;

void* malloc(size_t n);
void free(void* p);

void f(int a);
void g(int* a);
void h(const char* c);

void fc(int const a);
void gc(int* const a);
void hc(const char* const c);

//------------------------------------------------------
// Variables

void demo_stack_var() {
  int const x = 3;
  f(x);

  int z;
  z = 6;
  int const v = z;

  int y = 5;
  f(y);
  y = y + 1;
  y += 4;
}

void demo_malloc_var() {
  int* const a = (int*) malloc(sizeof(int));
  *a = *a + 2;
  g(a);
  free(a);
}

void demo_pointers() {
  int y = 1;
  int z = 2;

  int* const p = &y; // where p itself is pure
  *p = *p + 3;

  int* q = &y; // where q is mutated
  q = &z;
  *q = *q + 3;
}

//------------------------------------------------------
// Arrays

void demo_malloc_array() {
  int* const t = (int*) malloc(9 * sizeof(int));
  t[1] = t[1] + 2;
  free(t);

  int* u = (int*) malloc(9 * sizeof(int));
  u[1] = u[1] + 2;
  free(u);
  u = (int*) malloc(8 * sizeof(int));
  u[1] = u[1] + 2;
}

void demo_stack_array() {
  int const v[2] = { 0, 1 };
  f(v[1]);

  int w[2] = { 0, 1 };
  w[1] = w[1] + 2;
  f(w[1]);
  g(&w[1]);

  int* const a = &w[1];
  f(*a);
  g(a);

  int* b = &w[1];
  f(*b);
  g(b);
  b = &w[0];

  int* const r[2] = { w, w };
  gc(r[1]);
  fc(r[1][1]);

  int* s[2] = { w, w };
  g(r[1]);
  f(r[1][1]);
  s[1] = w;
}


//------------------------------------------------------
// Coercions

void demo_explicit_coercions() {
  bool b = true;
  int i = b;
  bool b2 = (bool) i;
  float r = (float) i;
  myint i2 = (myint) i;
}

void demo_implicit_coercions() {
  bool b = true;
  int i = b;
  bool b2 = i;
  size_t s = i;
  f(s);
  fc(s);
  myint j = i;
  f(j);
  fc(j);
}

void demo_arithop_coercions() {
  int i = 1;
  size_t s = 1;
  int r1 = i + s;
  size_t r2 = i + s;
  fc(i + s);
  myint j = 1;
  int r3 = i + j;
}

void demo_pointer_coercions() {
  int i = 1;
  g(&i);
  gc(&i);
}

//------------------------------------------------------
// Strings

void demo_strings() {
  char const x[10] = "foo";
  h("foo");
  h(x);
}


//------------------------------------------------------
// Records

typedef struct { int x; int y; } vect;

typedef struct { vect pos; vect speed; } particle;

typedef struct vects { vect head; struct vects* tail; } vects;

typedef vect* vectptr;

vect vect_mul(int f, vect const v) {
  return (vect) { f * v.x, f * v.y };
}

void demo_struct_init() {
  vect v = { 1, 2 };
  vect w1 = vect_mul(3, v);
  vect w2 = vect_mul(3, { 1, 2 });
  // vectptr vp = & ((vect) { 1, 2 });
  // vect w3 = vect_mul(3, *vp);
}


//------------------------------------------------------
//------------------------------------------------------
//------------------------------------------------------

/* TODO


typedef vect vect2;
typedef vect2 vect3;
typedef int int2[2];
typedef int* intstar;


void addr_array_cell() {
  int p[2];
  int* n = &p[0];
}

//------------------------------------------------------
// Loops

void test_loop() {
  int a = 0;
  for (int i = 0; i < 10; i++) {
      a++;
  }
  for (int i = 10; i >= 0; i--) {
      a--;
  }
  const int x = 3;
  const int y = 2;
  for (int x = 2; ; ) {
    int y = 1;
    int r = x + y;
  }
  const int z = x + y;

}



//------------------------------------------------------
// // Stack allocated variables are turned into heap allocated variables
void stack_var() {
  int r = 3;
  r = r + 1 + 2;
  r += 2;
  r++;
  int s = f(r);
}

// // Field and arrays accessed are composed in two steps:
// // first, compute the address of the memory cell, then derefence it.

// // For example,  t[i]  is decomposed as the operation that computes &t[i],
// // then applying the star operator to it, obtaining  "*(&t[i])".
// // The star is the standard dereferencing operator.
// // In the AST, the operation &t[i] is the application of the primitive
// // binary operation "Binop_array_cell_addr" to the argument t and i.

void stack_array() {
  int t[2] = { 5, 6 };
  int a = t[0]; // int a = *(t + 0)
  t[1] = a + 2;
}

// // Likewise,  t.x  is decomposed  as *(&t.x), which involves the primitive
// // unary operator "Unop_struct_field_addr x" applied to the argument t.

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

// // References: not yet implemented
void references() {
  int a = 3;
  int& b = a;
  b = b + 4;
}

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


// // When arrays are passed by values, they are not heap allocated.
// // They are values, out of which it is possible to directly read a value
// // using the operator Binop_array_cell_get.
// // Likewise a struct passed by value is read using Unop_struct_get.
// LATER:
void by_value(int t[2], vect v) { // implicit const arguments
  int b = t[0];
  int a = v.x;
  vect v2 = v; // this should make a copy of the data
}
// Not supported in OptiTrust
int g(int x) {
  x = x + 1;
  return x;
}


int h(int x) {
  int y = x + 1;
  return y;
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



void access_encoding() {
  const vect a = { 0, 1 };
  // copy as const
  const vect b = a;
  // copy as non-const
  vect c = a;
  // accesses to const
  const int ax = a.x;
 // accesses to non-const
  const int cy = c.y;
}

int foo(vect v) { return v.x; }

int mutable_var_encoding() {
    const vect a = { 0, 1 };
    int ax = foo(a);
    vect c = a;
    int cx = foo(c);
    return cx;
}

typedef struct {
  vect fst;
  vect snd;
} vectpair;

void lvalue_encoding() {
  vect* p;
  p->x = 2;
  (*p).x = 3;

  vectpair* q;
  q->fst.x = 2;
  (*q).fst.x = 3;

  int *v;
  *v = 4;
}

void arrow() {
 vect v = {0,1};
 vect* p = &v;
 (*p).x = (*p).y;
 p->x = p->y;
}

*/
